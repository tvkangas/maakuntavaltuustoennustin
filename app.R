##Kirjastot
library(readxl)
library(dplyr)
library(shiny)
library(plyr)


ui <- fluidPage(theme = "tyylitiedosto.css",
                
  navbarPage ("Maakuntavaltuustoennustin 3000",
  tabPanel(title = "Ennustin", id='ennustin',
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
    sliderInput("paikat",
                  "Paikkojen lukumäärä:",
                  min = 59,  max = 99, value = 59),
    
    selectInput("maakunta", "Maakunta:",
                choices = c('Keski-Suomi', 'Etelä-Savo', 'Kymenlaakso', 'Etelä-Karjala', 'Päijät-Häme',
                            'Kanta-Häme', 'Satakunta', 'Pohjois-Savo', 'Pohjois-Karjala', 'Lappi',
                            'Pirkanmaa', 'Pohjois-Pohjanmaa', 'Kainuu', 'Etelä-Pohjanmaa', 'Keski-Pohjanmaa',
                            'Pohjanmaa', 'Uusimaa', 'Varsinais-Suomi'
                )),
    radioButtons("menetelma", "Menetelmä:",
                 choices = c('Suoraan äänimääristä' = "suor", 'Optimointi' = "opt")),
    radioButtons("data", "Vaalidata:",
                choices = c('Kuntavaalit 2017' = 'kv', 'Eduskuntavaalit 2015' = 'edv'))
    ),
    mainPanel(
      #tableOutput("paikatPuolueet"),
      tableOutput("kunnat"),
      textOutput("teksti")
    )
  )
  ),
  tabPanel(title="Lisätietoja", id='lisat',
           mainPanel(
             h1("Maakuntavaltuustoennustin 3000"),
             p("Maakuntavaltuustoennustin 3000 on sovellus, jonka tarkoituksena on luoda erilaisia ennusteita maakuntauudistuksen mukana tulevista
               maakuntavaltuustoista. Käyttäjällä on mahdollisuus valita aineistoksi vuoden 2017 kuntavaalien tai vuoden 2015 eduskuntavaalien tulokset."),
             h2("Laskentamenetelmät"),
             p("Ennusteen voi määrittää kahdella tavalla. Menetelmä 'suoraan äänimääristä' määrittää valtuuston rakenteen siten, että valittu aineisto olisi maakuntavaalien tulos. Poikkeuksena on kuitenkin se, että äänestystulokset on 
anonymisoitu ehdokkaiden suhteen. Tämä vaikuttaa vain eduskuntavaaleihin, joissa sama ehdokas kerää ääniä koko vaalipiirissä. Laskelmassa kuitenkin huomioidaan erikseen ehdokkaan äänet yhdessä kunnassa. Tämän avulla ennuste viedään 
suuntaan, jossa arvioidaan puolueen potentiaalia yhden kunnan alueella."),
             p("Toinen menetelmä on 'optimointi menetelmä'. Tässä menetelmässä jokaisen kunnan sisällä puolueen äänet lasketaan yhteen, jolloin saadaan yksi äänipotti jokaista 'puolue-kunta'-paria kohden. Tämän jälkeen on luotu skenaario, jossa jokainen
kunta pyrkii optimoimaan äänien jakautumisen kunnan sisällä puolueen ehdokkaille siten, että kunta saisi ko puolueesta mahdollisimman monta ehdokasta maakuntavaltuustoon. Optimointi suoritetaan laskennallisesti D'Hondtin menetelmällä."),
             p("Maakuntavaltuuston puoluejakauma on määritetty D'Hondtin menetelmällä."),
             h2("Yleistä ja yhteystiedot"),
             p("Sovelluksen lähdekoodi, käytetty tyylitiedosto ja datatiedostot on julkaistu GitHubissa. GitHub-repositoryn linkki: https://github.com/tvkangas/maakuntavaltuustoennustin"),
             p("Sovelluksen kehittäjä: Tuukka Kangas"),
             p("Vaalidatan lähteet: Tilastokeskus, Oikeusministeriö"),
            p("Kuntien väkiluvut: Wikipedia.org"),
             p("Kysymykset ja kommentit: tuukkavkangas@gmail.com")

           )
       
           )
  )
)

#Luetaan kuntavaalidata tiedostosta
#Laskee äänet maakunnittain ja puolueittain. Lisätään äänimäärällä otsikko
#Tämän jälkeen helppouden vuoksi järjestetään maakunnittain ja äänimäärittäin
kvdata <- data.frame(read_excel("kuntavaalidata.xlsx", sheet = "data"))
edvdata <- data.frame(read_excel("eduskuntavaalidata.xlsx", sheet = "data"))

mkpuoaan_kv <- data.frame(aggregate(kvdata$Aanet, by=list(Maakunta=kvdata$Maakunta, Puolue=kvdata$Puolue), FUN=sum)) 
names(mkpuoaan_kv)[3]<-paste("Aanet")
mkpuoaan_kv <- mkpuoaan_kv[order(mkpuoaan_kv$Maakunta, -mkpuoaan_kv$Aanet),]
kunnatvakiluku <- data.frame(read_excel("suomenkunnatvaki.xlsx", sheet="data"))

mkpuoaan_edv <- data.frame(aggregate(edvdata$Aanet, by=list(Maakunta=edvdata$Maakunta, Puolue=edvdata$Puolue), FUN=sum)) 
names(mkpuoaan_edv)[3]<-paste("Aanet")
mkpuoaan_edv <- mkpuoaan_edv[order(mkpuoaan_edv$Maakunta, -mkpuoaan_edv$Aanet),]


####################
###Funktiot

#D'Hont
dHont <- function( ehdokkaat, aanet, paikat ){ 
  tmp <- data.frame( 
    ehdokkaat = rep( ehdokkaat, each = paikat ), 
    tulos     = as.vector(sapply( aanet, function(x) x / 
                                    1:paikat )) 
  ) 
  tmp <- tmp$ehdokkaat[order( - tmp$tulos )] [1:paikat] 
  table(tmp) 
} 
####################


server <- function(input, output, session) {
  
  output$kunnat <- renderTable({
    if (input$menetelma == "suor") {
      suoraanAanimaarista(input$maakunta, input$paikat)
    } else {
      optimointimenetelma(input$maakunta, input$paikat)  
    }
    
  })
  
  output$teksti <- renderText({
    if (input$menetelma == "suor") {
      df <- suoraanAanimaarista(input$maakunta, input$paikat)
    } else {
      df <- optimointimenetelma(input$maakunta, input$paikat)
    }
    
    kuntienvakiluvut <- dplyr::filter(kunnatvakiluku, Maakunta==input$maakunta)
    maakunnanvakiluku <- sum(kuntienvakiluvut$Vakiluku, dims=1)
    
    suurinkunta <- dplyr::filter(kunnatvakiluku, Kunta==df[2,1])
    suurimmanvakiluku <- suurinkunta[1,3]
    kunnatilman <- subset(df, Yhteensa == 0)
    kunnatilmanvakiluvut <- dplyr::filter(kuntienvakiluvut, Kunta %in% kunnatilman[,1])
    kunnatilmanvakiluku <- sum(kunnatilmanvakiluvut$Vakiluku, dims=1)


    if (df[2,(ncol(df))]/input$paikat > 0.5) {
      teksti <- paste("Suurin kunta ", df[2,1], " saa ennusteen mukaan yksinkertaisen enemmistön maakuntavaltuustoon.
                      Sen väkiluku on noin ", round(100*suurimmanvakiluku/maakunnanvakiluku,0), " % koko maakunnan väkiluvusta. Ennusteen mukaan 
                      ", df[2,1], " saa ", round(100*df[2,(ncol(df))]/input$paikat,0), " % maakuntavaltuuston paikoista.
                      Laskelman perusteella maakunnan kunnista ", nrow(kunnatilman), " jää ilman edustajaa maakuntavaltuustossa. 
                      Tämä vastaa ", round(100*nrow(kunnatilman)/(nrow(df)-1)), " % maakunnan kunnista. Asukkaita näissä kunnissa
                      on yhteensä ", kunnatilmanvakiluku, ", mikä vastaa ", round(100*kunnatilmanvakiluku/maakunnanvakiluku,0),
                      " % maakunnan väkiluvusta.",sep = "")
      
    } else {
      teksti <- paste("Yksikään kunta ei saa ennusteen mukaan yksinkertaista enemmistöä maakuntavaltuustoon. Eniten edustajia valtuustoon saa ",
                      df[2,1], ", jonka edustajat saavat ", round(100*df[2,(ncol(df))]/input$paikat,0), " % maakuntavaltuuston paikoista.
                      Laskelman perusteella maakunnan kunnista ", nrow(kunnatilman), " jää ilman edustajaa maakuntavaltuustossa. 
                      Tämä vastaa ", round(100*nrow(kunnatilman)/(nrow(df)-1)), " % maakunnan kunnista. Asukkaita näissä kunnissa
                      on yhteensä ", kunnatilmanvakiluku, ", mikä vastaa ", round(100*kunnatilmanvakiluku/maakunnanvakiluku,0),
                      " % maakunnan väkiluvusta." , sep="")
    }

  })
  
  puoluePaikat <- function(maara, maakunta) {
    if (input$data== "kv") {
      mkPuolueAanet <- dplyr::filter(mkpuoaan_kv,Maakunta==maakunta)
    } else {
      mkPuolueAanet <- dplyr::filter(mkpuoaan_edv,Maakunta==maakunta)
    }
    
    #Maakunnan paikat ja sarakkaiden nimeämistä
    paikatMaakunta <- data.frame(dHont(mkPuolueAanet[,2], mkPuolueAanet[,3], maara ) )
    names(paikatMaakunta)[1] <-paste("Puolue")
    names(paikatMaakunta)[2] <-paste("Paikat")
    
    #Suodatetaan pois sellaiset, joilla ei ole paikkoja ja järjestetään suuruusjärjestykseen
    paikatMaakunta <- dplyr::filter(paikatMaakunta ,Paikat > 0)
    paikatMaakunta <- paikatMaakunta [order(- paikatMaakunta$Paikat),]

    return (paikatMaakunta)
    
  }
  
  optimointimenetelma <- function(maakunta, paikat) {
    if (input$data== "kv") {
      maakuntadata <- dplyr::filter(kvdata, Maakunta==maakunta)
    } else {
      maakuntadata <- dplyr::filter(edvdata, Maakunta==maakunta)
    }
    
    kunnat <- maakuntadata[,("Kunta")]
    kunnat <- unique(kunnat)
    kunnat <- data.frame(kunnat)
    apu1 <- data.frame(matrix(nrow=1,ncol=1))
    apu1[1,1] <- "Yhteensa"
    names(apu1)[1] <- paste("Kunta")
    names(kunnat)[1] <- paste("Kunta")
    kunnat <- rbind(apu1,kunnat)
    # kunnat <- rbind(kunnat,apu1)
    puoluePaikat <- puoluePaikat(paikat, maakunta)
    taulu <- t(puoluePaikat)
    puolueet <- t(taulu[1,])
    
    tmptaulu <- matrix(nrow = length(kunnat), ncol = length(puolueet))
    colnames(tmptaulu) <- puolueet
    tmptaulu <- cbind(kunnat, tmptaulu)
    apu <- data.frame(matrix(nrow=1,ncol=1))
    colnames(apu)[1] <- paste("Yhteensa") 
    tmptaulu <- cbind(tmptaulu, apu)
    for (i in 3:ncol(tmptaulu)-1) { #Laittaa puolueen paikat
      riviindeksi <- which(puoluePaikat$Puolue == colnames(tmptaulu)[i])
      paikat <- puoluePaikat[riviindeksi,2]
      tmptaulu[1,i] = paste0("",paikat)
    }
    tmptaulu[, 2:ncol(tmptaulu)] <- sapply(tmptaulu[, 2:ncol(tmptaulu)], as.integer)
    
    for (i in 2:(ncol(tmptaulu)-1)) {
      puolue <- colnames(tmptaulu)[i]
      df <- dplyr::filter(maakuntadata, Puolue==puolue)
      aputaulu <- data.frame(aggregate(df$Aanet, by=list(Kunta=df$Kunta), FUN=sum))
      names(aputaulu)[2] <- paste("Aanet")
      maara <- puoluePaikat[which(puoluePaikat$Puolue == puolue),2]
      lapi <- data.frame(dHont(aputaulu[,1], aputaulu[,2],maara))
      colnames(lapi) <- c("Kunta", "Maara")
      
      for (j in 1:nrow(lapi)) {
        kunta <- lapi[j,1]
        maara <- lapi[j,2]
        sarakeindeksi <- grep(puolue, colnames(tmptaulu))
        riviindeksi <- which(tmptaulu$Kunta == kunta)
        tmptaulu[riviindeksi,sarakeindeksi] <- maara
      }
    }

    tmptaulu$Yhteensa <- rowSums(tmptaulu[,2:(ncol(tmptaulu)-1)], na.rm=TRUE)
    tmptaulu[is.na(tmptaulu)] <- 0
    tmptaulu[, 2:ncol(tmptaulu)] <- sapply(tmptaulu[, 2:ncol(tmptaulu)], as.integer)
    tmptaulu <- tmptaulu[order(-tmptaulu$Yhteensa),]
    tmptaulu[1,1] <- "Yhteensä"
    return(tmptaulu)
  }

  suoraanAanimaarista <- function(maakunta,paikat) {
    if (input$data== "kv") {
      maakuntadata <- dplyr::filter(kvdata, Maakunta==maakunta)
    } else {
      maakuntadata <- dplyr::filter(edvdata, Maakunta==maakunta)
    }
    
    #Haetaan kohdemaakunnan data
    maakuntadata <- dplyr::filter(kvdata, Maakunta==maakunta) 
    
    kunnat <- maakuntadata[,("Kunta")]
    kunnat <- unique(kunnat)
    kunnat <- data.frame(kunnat)
    apu1 <- data.frame(matrix(nrow=1,ncol=1))
    apu1[1,1] <- "Yhteensa"
    names(apu1)[1] <- paste("Kunta")
    names(kunnat)[1] <- paste("Kunta")
    kunnat <- rbind(apu1,kunnat)
    # kunnat <- rbind(kunnat,apu1)
    puoluePaikat <- puoluePaikat(paikat, maakunta)
    taulu <- t(puoluePaikat)
    puolueet <- t(taulu[1,])
    
    tmptaulu <- matrix(nrow = length(kunnat), ncol = length(puolueet))
    colnames(tmptaulu) <- puolueet
    tmptaulu <- cbind(kunnat, tmptaulu)
    apu <- data.frame(matrix(nrow=1,ncol=1))
    colnames(apu)[1] <- paste("Yhteensa") 
    tmptaulu <- cbind(tmptaulu, apu)
    for (i in 3:ncol(tmptaulu)-1) { #Laittaa puolueen paikat
      riviindeksi <- which(puoluePaikat$Puolue == colnames(tmptaulu)[i])
      paikat <- puoluePaikat[riviindeksi,2]
      tmptaulu[1,i] = paste0("",paikat)
    }
    tmptaulu[, 2:ncol(tmptaulu)] <- sapply(tmptaulu[, 2:ncol(tmptaulu)], as.integer)
    
    for (i in 2:(ncol(tmptaulu)-1)) {
      puolue <- colnames(tmptaulu)[i]
      df <- dplyr::filter(maakuntadata, Puolue==puolue)
      df <- df[order(-df$Aanet),]
      maara <- puoluePaikat[which(puoluePaikat$Puolue == puolue),2]
      lapi <- data.frame(table(select(df[1:maara,],Kunta)))
      colnames(lapi) <- c("Kunta", "Maara")
      
      for (j in 1:nrow(lapi)) {
        kunta <- lapi[j,1]
        maara <- lapi[j,2]
        sarakeindeksi <- grep(puolue, colnames(tmptaulu))
        riviindeksi <- which(tmptaulu$Kunta == kunta)
        tmptaulu[riviindeksi,sarakeindeksi] <- maara
      }
    }
    tmptaulu$Yhteensa <- rowSums(tmptaulu[,2:(ncol(tmptaulu)-1)], na.rm=TRUE)
    tmptaulu[is.na(tmptaulu)] <- 0
    tmptaulu[, 2:ncol(tmptaulu)] <- sapply(tmptaulu[, 2:ncol(tmptaulu)], as.integer)
    tmptaulu <- tmptaulu[order(-tmptaulu$Yhteensa),]
    tmptaulu[1,1] <- "Yhteensä"
    return(tmptaulu)
    
  }
}


shinyApp(ui = ui, server = server)

