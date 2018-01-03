# Ovde cu smestiti sve funkcije sa kratkim komentarom o izvrsavanju.



# Funkcija "ucitaj" sluzi za vracanje trivijalnih izvestaja za prosledjenu bazu po svakoj koloni: tip podatka, broj redova u koloni,
# broj na_vrednosti. I ako je ukljucen argument "benford" vraca rezultat hi-kvadrat testa za benforda.
# Takodje proslediti naziv baze, radi lepseg cuvanja fajla u argumentu "naziv_baze"
#   Argumenti:
#             baza - prosledjuje se baza nad kojom se izvrsava funkcija "ucitaj"
#             naziv_baze - naziv za izlazni fajl nakon izvrsavanja funkcije, ekstenzija .xlsx.
#             benford - logicka promenljiva, po defoltu je TRUE, tj izvrsava se benfordovo testiranje.
ucitaj <- function(baza, naziv_baze, benford = TRUE)
{
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(readr)) # nije obavezno
  if(benford)
  {  
    suppressPackageStartupMessages(library(BenfordTests))
    rezultat = matrix(ncol = ncol(baza), nrow = 4)
    dimnames(rezultat) = list(c('Tip', 'Duzina', 'Broj NA u R-u', 'Benford p-vrednost simulate asymptotic'), names(baza))
    
    for(i in names(baza))
    {
      # Pozivanje tipa
      rezultat[1, i] = typeof(baza[[i]])
      
      # Duzina
      rezultat[2, i] = length(baza[[i]])
      
      #na vrednosti
      rezultat[3, i] = sum(is.na(baza[[i]]))
      
      # benfordov_test
      if(is.numeric(baza[[i]]))
      {
        rezultat[4, i] = chisq.benftest(baza[[i]])[[2]]
      }
      else
      {
        rezultat[4, i] = "Nema smisla"
      }
    }
    
    ime = paste("/benford_za_bazu_", naziv_baze, sep = "")
    print(ime)
    file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep = "")
    res = write.xlsx(rezultat, file)  
  }
  if(benford == FALSE)
  {  
    rezultat = matrix(ncol = ncol(baza), nrow = 3)
    dimnames(rezultat) = list(c('Tip', 'Duzina', 'Broj NA u R-u'), names(baza))
    
    for(i in names(baza))
    {
      # Pozivanje tipa
      rezultat[1, i] = typeof(baza[[i]])
  
      # Duzina
      rezultat[2, i] = length(baza[[i]])
    
      #na vrednosti
      rezultat[3, i] = sum(is.na(baza[[i]]))
    }
    
    ime = paste("/za_bazu_", naziv_baze, sep = "")
    print(ime)
    file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep = "")
    res = write.xlsx(rezultat, file)  
  }
}


# Funkcija "interaktivniPlot" iscrtava interaktivni plot iz paketa "ggplot2" i "plotly". Treba DOVRSITI!
#   Argumenti: 
#     baza -  prosledjuje se dataset, celokupno
#     x_osa -  prosledjuje se kolona iz baze "baza" koja se iscrtava na x-osi.
#     y_osa -  prosledjuje se kolona iz baze "baza" koja se iscrtava na y-osi.
#     naziv_x_osa -  prosledjuje se naziv x-ose, koja se ispisuje.
#     naziv_y_osa -  prosledjuje se naziv y-ose, koja se ispisuje.
#     prom_velicina -  prosledjuje se kolona iz baze "baza" pomocu koje se skalira tacka.
#     naziv_prom_velicina -  prosledjuje se naziv promenljive koja skalira tacke.

interaktivniPlot <-function(baza, x_osa, y_osa, naziv_x_osa, naziv_y_osa, prom_velicina, naziv_prom_velicina)
{
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  interaktivni_plot = ggplot(baza, aes(x = x_osa, y = y_osa, size = prom_velicina))+
                      geom_point(aes(text = paste(naziv_prom_velicina,baza[[1]]))) +
                      scale_x_continuous(name = paste(naziv_x_osa))+
                      scale_y_continuous(name = paste(naziv_y_osa))+
                      scale_size(naziv_prom_velicina, range = c(1,7))+
                      #scale_radius(range=c(1,20), colors(distinct = TRUE), guide = "legend")+
                      xlab(paste(naziv_x_osa, ":")) + 
                      ylab(paste(naziv_y_osa, ":"))
  ggplotly(interaktivni_plot)
  return(interaktivni_plot)
}

# Funkcija "remove_outliers" vadi sve autlajera iz prosledjene kolone (argument "x").
# Poziva se u funkciji "plotovanje".
remove_outliers <- function(x, na.rm = TRUE, ...) 
{
  qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - H)] = NA
  y[x > (qnt[2] + H)] = NA
  return(y)
}


# Funkcija "plotovanje" iscrtava grafike histograma, gustine i (opciono) boks-plota, za sve kolone iz prosledjene baze. Treba dovrsiti!
#   Argumenti:
#     baza - prosledjuje se dataset za cije sve kolone se iscrtavaju odgovarajuci grafici.
#     bez_autlajera - TRUE/FALSE vrednosti, da li da iscrtava grafike sa ukljucenim autlajerima ili ipak bez njih. Ukoiko TRUE - bez boks-plota.
#   Poziva funkciju remove_outliers, ako je "bez_autlajera==TRUE".
plotovanje <- function(baza, bez_autlajera)
{
  
  for(i in names(baza))
  {
    if(is.numeric(baza[[i]]))
    {
      if(bez_autlajera)
      {
        par(mfrow = c(1,2))
        kolona = remove_outliers(baza[[i]])
      }
      else
      {
        par(mfrow = c(1,3))
        kolona = baza[[i]]
      }
      
      histogram = hist(kolona, xlab = i, main = paste("Histogram ", i, sep = ""), ylab = "Ucestalost", probability = TRUE)
      gustina = plot(density(na.omit(kolona)), main = paste("Gustina kolone ", i, sep = ""), xlab = i, ylab = "Gustina")
      if(bez_autlajera == FALSE)
      {
        boksplot = boxplot(kolona)
      }
    }
  }
}


# Funkcija "plotuj_celu_bazu" plotuje i cuva u .pdf fajlu plotove iz funkcije "plotovanje".
#   Argumenti:
#     baza - prosledjuje dataset.
#     naziv_baze - prosledjuje se radi boljeg cuvanja fajla.
#     bez_autlajera - TRUE/FALSE vrednosti, se prosledjuje u funkciju "plotovanje".
plotuj_celu_bazu <- function(baza, naziv_baze, bez_autlajera = TRUE)
{
  ime = paste("C:/Eksport/Rezultati/Raspodele_", naziv_baze, ".pdf", sep = "")
  pdf(ime)
  plotovanje(baza, bez_autlajera)
  dev.off()
}



# Funkcija "primeni_ks_test" primenjuje Kolmogorov-Smirnov test za proveru da li su kolone iz baze normalno raspodeljene i cuva 
# u eksel fajlu.
#   Argumenti:
#     baza - prosledjuje bazu (dataframe).
#     naziv_baze -  prosledjuje se radi boljeg cuvanja fajla.
#     bez_autlajera - TRUE/FALSE vrednosti, ako je TRUE iskljucuje se autljaeri pozivom funkcije "remove_outliers".
primeni_ks_test <- function(baza, naziv_baze, bez_autlajera = FALSE)
{
  suppressPackageStartupMessages(library(xlsx))
  if(bez_autlajera == FALSE)
  {
    rezultat = matrix(ncol = ncol(baza), nrow = 1)
    dimnames(rezultat) = list(c("p-vrednost"), names(baza))
    
    for(i in names(baza))
    {
      if(is.numeric(baza[[i]]))
      {
        rezultat[1, i] = ks.test(baza[[i]], "pnorm")[[2]] # to je p-vred
      }
    }
    ime = paste("/ks_test_za_bazu_sa_autlajerima", naziv_baze, sep = "")
    print(ime)
    file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep = "")
    res = write.xlsx(rezultat, file)  
  }
  
  if(bez_autlajera)
  {
    rezultat = matrix(ncol = ncol(baza), nrow = 1)
    dimnames(rezultat) = list(c("p-vrednost"), names(baza))
    
    for(i in names(baza))
    {
      if(is.numeric(baza[[i]]))
      {
        kolona = remove_outliers(baza[[i]])
        rezultat[1, i] = ks.test(kolona, "pnorm")[[2]] # to je p-vred
      }
      
    }
    ime = paste("/ks_test_bez_autlajera_za_bazu_", naziv_baze, sep = "")
    print(ime)
    file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep = "")
    res = write.xlsx(rezultat, file)  
  }
}

# Funkcija "savePlot" - cuva prosledjeni plot sa prosledjenim imenom.
#   Argumenti:
#     myPlot - plot koji se salje.
#     ime - kako se da se cuva.
savePlot <- function(myPlot, ime) 
{
  pdf(ime)
  print(myPlot)
  dev.off()
}


# Kolicina prodatih stvari po mesecu za radnika
#   Argumenti:
#     baza - pozeljno da se prosledi sortirani po vremenu dataset.
#     Radnici - nalozi svih radnika, koji postoji u prosledjenom dataset-u
#     naziv_prom_radnika - string vrednost, koja pokazuje kako se u bazi zove kolona, koja belezi radnika.
#     naziv_y_osa - string vrednost, koja pokazuje sta zelimo da plotujemo na y- osi (Kolicina).
#     datum_prom - string vrednost, naziv datumske kolone u bazi, iz koje se izdvaja mesec (x-osa). (DAY_DATE) y fact_racuni.
kolicinaProdatogPoRadniku <- function(baza, Radnici, naziv_prom_radnika, naziv_y_osa, datumska_prom)
{
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  for(j in Radnici)
  {
    nova_baza_radnik0 = baza[which(baza[c(naziv_prom_radnika)] == j), ]
    aggdata_radnik0 = aggregate(nova_baza_radnik0[c(naziv_y_osa)], by = list(month = substr(nova_baza_radnik0[c(datumska_prom)], 1, 7) ), 
                              FUN = sum, na.rm = TRUE)
    title = paste("Kolicina prodatih stvari po mesecu za radnika", j, sep = " ")
    aggdata_radnik0 = cbind(aggdata_radnik0,unique(substr(nova_baza_radnik0[c(datumska_prom)], 1, 7))  )
    flevels = levels(aggdata_radnik0[c(datumska_prom)])

    plot_neki = ggplot(aggdata_radnik0, aes(x = aggdata_radnik0[c(datumska_prom)], y = aggdata_radnik0[c(naziv_y_osa)])) +
                geom_col(aes(fill = aggdata_radnik0[c(naziv_y_osa)])) +
                scale_x_discrete(limits = flevels,name = "Mesec i godina") +
                scale_y_continuous(name = "Kolicina prodatih stvari po mesecu") +
                ggtitle(title) +
                scale_fill_gradient2("Kolicina", low = "black", mid = "lightblue", high = "red") +
                theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 10))
    #plot_neki +  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
    ggplotly(plot_neki)
    ime = paste("C:/Eksport/Rezultati/Kolicina_prodatog/kolicina_prodaje_radnika_", j, ".pdf", sep = "")
    savePlot(plot_neki, ime)
  }
}

### PRIMER:
baza =  fact_racuni[order(fact_racuni$HOUR_DATE), ]
radnici = unique(baza$KORISNIK)
kolicinaProdatogPoRadniku(baza = baza, Radnici = radnici, naziv_prom_radnika = "KORISNIK", naziv_y_osa = "KOLICINA", datumska_prom = "DAY_DATE")
###
a = "KORISNIK"
fact_racuni$a

######################################################
########### VREMENSKE SERIJE ZA ORGJED ###############
######################################################
# Nije bas u obliku funkcije, zbog slozenosti uopstavanja.

## Promet za svaki mesec sa predvidjanjem na 12 meseci.
#
#   Napomena:
#     ucitati bazu "fact_racuni"
vremenskeSerijeMP_VREDNOSTmesecno <- function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza =  fact_racuni[order(fact_racuni$HOUR_DATE), ]

  suppressPackageStartupMessages(library(forecast))
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(reshape2))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(lubridate))

  ime = paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Vremenska_serija_promet_orgjed_predvidjanje.pdf", sep="")
  pdf(ime)
  for(i in Radnje)
  {
    nova_baza = baza[which(baza$ORGJED ==i),]
    # nova_baza = cbind(nova_baza, nova_baza$MP_VREDNOST - nova_baza$NABAVNA_VREDNOST - nova_baza$POPUST_IZNOS)
    em = melt(nova_baza[c('MP_VREDNOST',  'ORGJED', 'DAY_DATE')],id=2:3 )
    aggdata = aggregate(em[c('value')],by=list(month = substr(em$DAY_DATE, 1, 7), em$ORGJED ), FUN=sum, na.rm=TRUE)
    aggdata = cbind(aggdata, substr(aggdata[[1]], 1, 4))
    flevels = c()
    for(j in 1:(floor(length(aggdata[,1])/6)) )
    {
    
      flevels =c(flevels, aggdata[,1][6*j])
    }      
  
    plot_neki = ggplot(data = aggdata,aes(x=aggdata[,1], y=aggdata[,3], geom = "line", group = aggdata[,2], color=aggdata[,2])) +
                      geom_line()+
                      facet_grid(aggdata[,2] ~ ., scale = "free_y") +
                      scale_x_discrete(name = "Vreme", breaks = flevels) +
                      scale_y_continuous(name = "Promet", breaks= seq(min(aggdata[,3]), max(aggdata[,3]), 100000)) +
                      theme_bw()+
                      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
    print(plot_neki)
    mesec_godina = paste(aggdata[,1][1], '01', sep = '-')
    vremenska = ts(aggdata[,3], frequency = 12, start = c(year( paste(aggdata[,1][1], '01', sep = '-')), month( paste(aggdata[,1][1], '01', sep = '-'))))
    if(length(vremenska)>24)
    {
      #plot.ts(vremenska)
      plot(na.omit(decompose(vremenska)))
      print(acf(vremenska))
      print(pacf(vremenska))
      print(acf(na.omit(decompose(vremenska)$random)))
      najbolji_model = auto.arima(na.omit(decompose(vremenska)$random))
      print(plot(forecast(vremenska,12)))
      write.xlsx(forecast(vremenska, 12), 
                 file =paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Vremenska_serija_promet_orgjed_predvidjanje_", i, ".xlsx", sep="") )
      istorija = cbind(aggdata[,1], vremenska)
      write.xlsx(istorija, 
                 file =paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Vremenska_serija_promet_orgjed_istorija_", i, ".xlsx", sep="") )
    }
  }
  dev.off()
}

vremenskeSerijeMP_VREDNOSTmesecno()
## Kolicina za svaki mesec sa predvidjanjem na 12 meseci.
#   Napomena:
#     ucitati - fact_racuni.
vremenskeSerijeKOLICINAmesecno <-function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza =  fact_racuni[order(fact_racuni$HOUR_DATE),]
  
  suppressPackageStartupMessages(library(forecast))
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(reshape2))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(lubridate))
  
  ime=paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Vremenska_serija_zaKolicinu_orgjed", "pdf", sep=".")
  pdf(ime)
  for(i in Radnje)
  {
  
    nova_baza = baza[which(baza$ORGJED == i),]
    em= melt(nova_baza[c('KOLICINA', 'ORGJED', 'DAY_DATE')], id=2:3 )
    aggdata=aggregate(em[c('value')],by=list(month = substr(em$DAY_DATE, 1, 7), em$ORGJED ), FUN = sum, na.rm=TRUE)
    aggdata = cbind(aggdata, substr(aggdata[[1]], 1, 4))
    flevels = c()
    for(j in 1:(floor(length(aggdata[,1])/6)) )
    {
      flevels =c(flevels, aggdata[,1][6*j])
    }      
  
    plot_neki = ggplot(data = aggdata,aes(x=aggdata[,1], y=aggdata[,3], geom = "line", group = aggdata[,2], color=aggdata[,2])) +
                geom_line()+
                facet_grid(aggdata[,2] ~ ., scale = "free_y") +
                scale_x_discrete(name = "Vreme", breaks = flevels) +
                scale_y_continuous(name = "Kolicina", breaks= seq(min(aggdata[,3]), max(aggdata[,3]), 500)) +
                theme_bw()+
                theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
    print(plot_neki)
    mesec_godina = paste(aggdata[,1][1], '01', sep = '-')
    vremenska = ts(aggdata[,3], frequency = 12, start = c(year( paste(aggdata[,1][1], '01', sep = '-')), month( paste(aggdata[,1][1], '01', sep = '-'))))
    if(length(vremenska)>24)
    {
      plot(na.omit(decompose(vremenska)))
      print(acf(vremenska))
      print(pacf(vremenska))
      print(acf(na.omit(decompose(vremenska)$random)))
      najbolji_model = auto.arima(aggdata[3])
      print(plot(forecast(vremenska,12)  ))
      write.xlsx(forecast(vremenska, 12),
                 file = paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Vremenska_serija_zaKolicinu_orgjed_", i, ".xlsx", sep="") )
      istorija = cbind(aggdata[,1], vremenska)
      write.xlsx(istorija, 
                 file =paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Vremenska_serija_zaKolicinu_orgjed_istorija_", i, ".xlsx", sep="") )
    }
  }
  dev.off()
}

vremenskeSerijeKOLICINAmesecno()



# Nije bas u obliku funkcije, zbog slozenosti uopstavanja.

## Promet za svaki dan sa predvidjanjem na 365 dana.
#
#   Napomena:
#     ucitati bazu "fact_racuni"
vremenskeSerijeMP_VREDNOSTdnevno <- function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza =  fact_racuni[order(fact_racuni$HOUR_DATE),]
  
  suppressPackageStartupMessages(library(forecast))
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(reshape2))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(lubridate))
  
  ime = paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Dnevno/Vremenska_serija_promet_orgjed_predvidjanje.pdf", sep=".")
  pdf(ime)
  for(i in Radnje)
  {
    nova_baza = baza[which(baza$ORGJED ==i),]
    # nova_baza = cbind(nova_baza, nova_baza$MP_VREDNOST - nova_baza$NABAVNA_VREDNOST - nova_baza$POPUST_IZNOS)
    
    em = melt(nova_baza[c('MP_VREDNOST',  'ORGJED', 'DAY_DATE')],id=2:3 )
    aggdata = aggregate(em[c('value')],by=list(month = substr(em$DAY_DATE, 1, 10), em$ORGJED ), FUN=sum, na.rm=TRUE)
    aggdata = cbind(aggdata, substr(aggdata[[1]], 1, 4))
    flevels = c()
    for(j in 1:(floor(length(aggdata[,1])/60)) )
    {
      
      flevels = c(flevels, aggdata[,1][60*j])
    }      
    
    plot_neki = ggplot(data = aggdata,aes(x=aggdata[,1], y=aggdata[,3], geom = "line", group = aggdata[,2], color=aggdata[,2])) +
                geom_line()+
                facet_grid(aggdata[,2] ~ ., scale = "free_y") +
                scale_x_discrete(name = "Vreme", breaks = flevels) +
                scale_y_continuous(name = "Promet", breaks= seq(min(aggdata[,3]), max(aggdata[,3]), 100000)) +
                theme_bw()+
                theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
    print(plot_neki)
    vremenska = ts(aggdata[,3], frequency = 365, start = c(year(aggdata[,1][1]), month( aggdata[,1][1]), day(aggdata[,1][1])))
    if(length(vremenska)>2*365)
    {
      #plot.ts(vremenska)
      plot(na.omit(decompose(vremenska)))
      print(acf(vremenska))
      print(pacf(vremenska))
      print(acf(na.omit(decompose(vremenska)$random)))
      najbolji_model = auto.arima(na.omit(decompose(vremenska)$random))
      print(plot(forecast(vremenska,365)))
      write.xlsx(forecast(vremenska, 365), 
                 file =paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Dnevno/Vremenska_serija_promet_orgjed_predvidjanje_", i, ".xlsx", sep="") )
      istorija = cbind(aggdata[,1], vremenska)
      write.xlsx(istorija, 
                 file =paste("C:/Eksport/Rezultati/Vremenske_serije/MP_VREDNOST/Dnevno/Vremenska_serija_promet_orgjed_istorija_", i, ".xlsx", sep="") )
    }
  }
  dev.off()
}

vremenskeSerijeMP_VREDNOSTdnevno()
## Kolicina za svaki mesec sa predvidjanjem na 12 meseci.
#   Napomena:
#     ucitati - fact_racuni.
vremenskeSerijeKOLICINAdnevno <-function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza =  fact_racuni[order(fact_racuni$HOUR_DATE),]
  
  suppressPackageStartupMessages(library(forecast))
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(reshape2))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(lubridate))
  
  ime = paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Dnevno/Vremenska_serija_zaKolicinu_orgjed", "pdf", sep=".")
  pdf(ime)
  for(i in Radnje)
  {
    
    nova_baza = baza[which(baza$ORGJED == i),]
    em = melt(nova_baza[c('KOLICINA', 'ORGJED', 'DAY_DATE')],id=2:3 )
    aggdata = aggregate(em[c('value')],by=list(month = substr(em$DAY_DATE, 1, 10), em$ORGJED ), FUN=sum, na.rm=TRUE)
    aggdata = cbind(aggdata, substr(aggdata[[1]], 1, 4))
    flevels = c()
    for(j in 1:(floor(length(aggdata[,1])/60)) )
    {
      flevels = c(flevels, aggdata[,1][60*j])
    }      
    
    plot_neki = ggplot(data = aggdata,aes(x=aggdata[,1], y=aggdata[,3], geom = "line", group = aggdata[,2], color=aggdata[,2])) +
                geom_line()+
                facet_grid(aggdata[,2] ~ ., scale = "free_y") +
                scale_x_discrete(name = "Vreme", breaks = flevels) +
                scale_y_continuous(name = "Kolicina", breaks= seq(min(aggdata[,3]), max(aggdata[,3]), 500)) +
                theme_bw()+
                theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
    print(plot_neki)
    vremenska = ts(aggdata[,3], frequency = 365, start = c(year(aggdata[,1][1]), month( aggdata[,1][1]), day(aggdata[,1][1])))
    if(length(vremenska)>2*365)
    {
      plot(na.omit(decompose(vremenska)))
      print(acf(vremenska))
      print(pacf(vremenska))
      print(acf(na.omit(decompose(vremenska)$random)))
      najbolji_model = auto.arima(aggdata[3])
      print(plot(forecast(vremenska,365)  ))
      write.xlsx(forecast(vremenska, 365), 
                 file = paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Dnevno/Vremenska_serija_zaKolicinu_orgjed_", i, ".xlsx", sep="") )
      istorija = cbind(aggdata[,1], vremenska)
      write.xlsx(istorija, 
                 file = paste("C:/Eksport/Rezultati/Vremenske_serije/Kolicina/Dnevno/Vremenska_serija_zaKolicinu_orgjed_istorija_", i, ".xlsx", sep="") )
    }
  }
  dev.off()
}

vremenskeSerijeKOLICINAdnevno()


########################################################################
############ Analiza prodaje na dnevnom nivou za svaki orgjed ##########
############ radi uocavanja alarmantnih situacija ######################
########################################################################

# Pozivi ispod racunaju koliko koja ORGJED iz fact_racuni dnevno prodaje stvari, koliko ih vraca i koliki je zbir te dve stvari.
# Vraca .XLSX fajl sa tim informacijama.

# Cilj je da se pomocu toga optimizuje pronazak alarmantnih situacija i dana, koji ne upadaju u 99% studentov interval poverenja.

#   Napomena:
#     ucitati - fact_racuni.
osnovniPodaciKOLICINA <- function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza = fact_racuni[order(fact_racuni$HOUR_DATE), ]
  suppressPackageStartupMessages(library(xlsx))

  cgt_matrica_kolicina = matrix(ncol = length(radnje), nrow  = 20)
  dimnames(cgt_matrica_kolicina) = list(c('Srednja vrednost', 'Std odstupanje', 'Obim', 
                                          'Donja granica intervala poverenja','Gornja granica intervala poverenja', 
                                          'Koeficijent', ' PODACI ZA POVRACAJ', 'Srednja vrednost_povracaj', 
                                          'Std odstupanje_povracaj', 'Obim_povracaj', 'Donja granica intervala poverenja_povracaj', 
                                          'Gornja granica intervala poverenja_povracaj', 'Koeficijent_povracaj',' PODACI ZA PRODAJU', 
                                          'Srednja vrednost_prodaja','Std odstupanje_prodaja', 'Obim_prodaja', 
                                          'Donja granica intervala poverenja_prodaja','Gornja granica intervala poverenja_prodaja', 
                                          'Koeficijent_prodaja' ), radnje)
  ime = paste("/za_orgjed_kolicina_prodaje_po_danu2", sep="")
  print(ime)

  for(i in Radnje)
  {
    nova_baza = baza[which(baza$ORGJED == i), ]
    nova_baza_povracaj  = baza[which(baza$ORGJED == i & baza$KOLICINA < 0), ]
    nova_baza_prodaja  = baza[which(baza$ORGJED == i & baza$KOLICINA > 0), ]
    aggdata = aggregate(nova_baza[c('KOLICINA')], by = list(day = substr(nova_baza$DAY_DATE, 1, 10) ), 
                        FUN = sum, na.rm = TRUE)
    aggdata_povracaj = aggregate(nova_baza_povracaj[c('KOLICINA')], by = list(day = substr(nova_baza_povracaj$DAY_DATE, 1, 10) ), 
                                 FUN = sum, na.rm = TRUE)
    aggdata_prodaja = aggregate(nova_baza_prodaja[c('KOLICINA')], by = list(day = substr(nova_baza_prodaja$DAY_DATE, 1, 10) ), 
                                FUN = sum, na.rm = TRUE)
  
    # Trebalo bi da vazi CGT na sve ovo. Sve KOLICINE
    cgt_matrica_kolicina[1, i] = mean(aggdata$KOLICINA)
    cgt_matrica_kolicina[2, i] = sqrt(var(aggdata$KOLICINA))
    cgt_matrica_kolicina[3, i] = length(aggdata$KOLICINA)
    # donja granica  95% interval poverenja
    cgt_matrica_kolicina[4, i] = cgt_matrica_kolicina[1,i] - qt(0.995, cgt_matrica_kolicina[3, i] - 1) * cgt_matrica_kolicina[2, i]/ sqrt(cgt_matrica_kolicina[3, i])
    # gornja granica 95% interval poverenja
    cgt_matrica_kolicina[5, i] = cgt_matrica_kolicina[1,i] + qt(0.995, cgt_matrica_kolicina[3, i] - 1) * cgt_matrica_kolicina[2, i]/ sqrt(cgt_matrica_kolicina[3, i])            
    cgt_matrica_kolicina[6, i] = (cgt_matrica_kolicina[5, i] - cgt_matrica_kolicina[4, i])/cgt_matrica_kolicina[1, i]*sqrt(cgt_matrica_kolicina[3, i])
  
    cgt_matrica_kolicina[7, i] = 0
  
    # Slicno za povrat kolicine
    if(length(aggdata_povracaj$KOLICINA) > 2)
      cgt_matrica_kolicina[8, i] = mean(aggdata_povracaj$KOLICINA)
    cgt_matrica_kolicina[9, i] = sqrt(var(aggdata_povracaj$KOLICINA))
    cgt_matrica_kolicina[10, i] = length(aggdata_povracaj$KOLICINA)
    # donja granica  95% interval poverenja
    cgt_matrica_kolicina[11, i] = cgt_matrica_kolicina[8,i] - qt(0.995, cgt_matrica_kolicina[10, i] - 1) * cgt_matrica_kolicina[9, i]/ sqrt(cgt_matrica_kolicina[10, i]) 
    # gornja granica 95% interval poverenja
    cgt_matrica_kolicina[12, i] = cgt_matrica_kolicina[8,i] + qt(0.995, cgt_matrica_kolicina[10, i] - 1) * cgt_matrica_kolicina[9, i]/ sqrt(cgt_matrica_kolicina[10, i])            
    cgt_matrica_kolicina[13, i] = (cgt_matrica_kolicina[12, i] - cgt_matrica_kolicina[11, i])/cgt_matrica_kolicina[8, i]*sqrt(cgt_matrica_kolicina[10, i])  
  
    cgt_matrica_kolicina[14, i] = 0
  
    # I za bas prodaju
    cgt_matrica_kolicina[15, i] = mean(aggdata_prodaja$KOLICINA)
    cgt_matrica_kolicina[16, i] = sqrt(var(aggdata_prodaja$KOLICINA))
    cgt_matrica_kolicina[17, i] = length(aggdata_prodaja$KOLICINA)
    cgt_matrica_kolicina[18, i] = cgt_matrica_kolicina[15,i] - qt(0.995, cgt_matrica_kolicina[17, i] - 1) * cgt_matrica_kolicina[16, i]/ sqrt(cgt_matrica_kolicina[17, i])
    cgt_matrica_kolicina[19, i] = cgt_matrica_kolicina[15,i] + qt(0.995, cgt_matrica_kolicina[17, i] - 1) * cgt_matrica_kolicina[16, i]/ sqrt(cgt_matrica_kolicina[17, i])
    cgt_matrica_kolicina[20, i] = (cgt_matrica_kolicina[19, i] - cgt_matrica_kolicina[18, i])/cgt_matrica_kolicina[15, i]*sqrt(cgt_matrica_kolicina[17, i]) 
  }
  file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep="")
  res = write.xlsx(cgt_matrica_kolicina, file) 
}



# Pozivi ispod racunaju koliko koja ORGJED iz fact_racuni dnevno pravi pazara, koliko ih vraca i koliki je zbir te dve stvari.
# Vraca .XLSX fajl sa tim informacijama.

# Cilj je da se pomocu toga optimizuje pronazak alarmantnih situacija i dana, koji ne upadaju u 99% studentov interval poverenja.

#   Napomena:
#     ucitati - fact_racuni.
osnovniPodaciMP_VREDNOST <- function()
{
  Radnje = unique(fact_racuni$ORGJED)
  baza = fact_racuni[order(fact_racuni$HOUR_DATE), ]
  suppressPackageStartupMessages(library(xlsx))

  cgt_matrica_kolicina = matrix(ncol = length(radnje), nrow  = 20)
  dimnames(cgt_matrica_kolicina) = list(c('Srednja vrednost', 'Std odstupanje', 'Obim', 'Donja granica intervala poverenja', 
                                          'Gornja granica intervala poverenja', 'Koeficijent', ' PODACI ZA POVRACAJ', 
                                          'Srednja vrednost_povracaj','Std odstupanje_povracaj', 
                                          'Obim_povracaj', 'Donja granica intervala poverenja_povracaj', 
                                          'Gornja granica intervala poverenja_povracaj', 'Koeficijent_povracaj', 
                                          ' PODACI ZA PRODAJU', 'Srednja vrednost_prodaja', 'Std odstupanje_prodaja', 
                                          'Obim_prodaja', 'Donja granica intervala poverenja_prodaja',
                                          'Gornja granica intervala poverenja_prodaja', 'Koeficijent_prodaja' ), radnje)
  ime = paste("/za_orgjed_pazar_po_danu2", sep = "")
  print(ime)

  for(i in Radnje)
  {
    nova_baza = baza[which(baza$ORGJED == i), ]
    nova_baza_povracaj  = baza[which(baza$ORGJED == i & baza$MP_VREDNOST < 0), ]
    nova_baza_prodaja  = baza[which(baza$ORGJED == i & baza$MP_VREDNOST > 0), ]
    aggdata = aggregate(nova_baza[c('MP_VREDNOST')], by = list(day = substr(nova_baza$DAY_DATE, 1, 10) ), 
                        FUN = sum, na.rm = TRUE)
    aggdata_povracaj = aggregate(nova_baza_povracaj[c('MP_VREDNOST')], by = list(day = substr(nova_baza_povracaj$DAY_DATE, 1, 10) ), 
                                 FUN = sum, na.rm = TRUE)
    aggdata_prodaja = aggregate(nova_baza_prodaja[c('MP_VREDNOST')], by = list(day = substr(nova_baza_prodaja$DAY_DATE, 1, 10) ), 
                                FUN = sum, na.rm = TRUE)
  
    # Trebalo bi da vazi CGT na sve ovo. Sve MP_VREDNOST-i
    cgt_matrica_kolicina[1, i] = mean(aggdata$MP_VREDNOST)
    cgt_matrica_kolicina[2, i] = sqrt(var(aggdata$MP_VREDNOST))
    cgt_matrica_kolicina[3, i] = length(aggdata$MP_VREDNOST)
    cgt_matrica_kolicina[4, i] = cgt_matrica_kolicina[1, i] - qt(0.995, cgt_matrica_kolicina[3, i] - 1) * cgt_matrica_kolicina[2, i]/ sqrt(cgt_matrica_kolicina[3, i])        # donja granica  95% interval poverenja
    cgt_matrica_kolicina[5, i] = cgt_matrica_kolicina[1, i] + qt(0.995, cgt_matrica_kolicina[3, i] - 1) * cgt_matrica_kolicina[2, i]/ sqrt(cgt_matrica_kolicina[3, i])            # gornaj granica 95% interval poverenja
    cgt_matrica_kolicina[6, i] = (cgt_matrica_kolicina[5, i] - cgt_matrica_kolicina[4, i])/cgt_matrica_kolicina[1, i]*sqrt(cgt_matrica_kolicina[3, i])
  
    cgt_matrica_kolicina[7, i] = 0
  
    # Slicno za povrat MP_VREDNOST
  
    cgt_matrica_kolicina[8, i] = mean(aggdata_povracaj$MP_VREDNOST)
    cgt_matrica_kolicina[9, i] = sqrt(var(aggdata_povracaj$MP_VREDNOST))
    cgt_matrica_kolicina[10, i] = length(aggdata_povracaj$MP_VREDNOST)
    cgt_matrica_kolicina[11, i] = cgt_matrica_kolicina[8, i] - qt(0.995, cgt_matrica_kolicina[10, i] - 1) * cgt_matrica_kolicina[9, i]/ sqrt(cgt_matrica_kolicina[10, i])        # donja granica  95% interval poverenja
    cgt_matrica_kolicina[12, i] = cgt_matrica_kolicina[8, i] + qt(0.995, cgt_matrica_kolicina[10, i] - 1) * cgt_matrica_kolicina[9, i]/ sqrt(cgt_matrica_kolicina[10, i])            # gornaj granica 95% interval poverenja
    cgt_matrica_kolicina[13, i] = (cgt_matrica_kolicina[12, i] - cgt_matrica_kolicina[11, i])/cgt_matrica_kolicina[8, i]*sqrt(cgt_matrica_kolicina[10, i])  
  
    cgt_matrica_kolicina[14, i] = 0
  
    # I za bas prodaju MP_VREDNOST
    cgt_matrica_kolicina[15, i] = mean(aggdata_prodaja$MP_VREDNOST)
    cgt_matrica_kolicina[16, i] = sqrt(var(aggdata_prodaja$MP_VREDNOST))
    cgt_matrica_kolicina[17, i] = length(aggdata_prodaja$MP_VREDNOST)
    cgt_matrica_kolicina[18, i] = cgt_matrica_kolicina[15, i] - qt(0.995, cgt_matrica_kolicina[17, i] - 1) * cgt_matrica_kolicina[16, i]/ sqrt(cgt_matrica_kolicina[17, i])        # donja granica  95% interval poverenja
    cgt_matrica_kolicina[19, i] = cgt_matrica_kolicina[15, i] + qt(0.995, cgt_matrica_kolicina[17, i] - 1) * cgt_matrica_kolicina[16, i]/ sqrt(cgt_matrica_kolicina[17, i])            # gornaj granica 95% interval poverenja
    cgt_matrica_kolicina[20, i] = (cgt_matrica_kolicina[19, i] - cgt_matrica_kolicina[18, i])/cgt_matrica_kolicina[15, i]*sqrt(cgt_matrica_kolicina[17, i]) 
  }
  file = paste("C:/Eksport/Rezultati", ime, ".xlsx", sep = "")
  res = write.xlsx(cgt_matrica_kolicina, file) 
}

#########################
#########################
# Algoritmi asocijacije #
#########################
#########################

# Funkcija "primeniApriori" - primenjuje apriori algoritam na zadatu listu "lista".
#   Argumenti:
#     lista - lista podataka na koju se primenjuje apriori algoritam.
#     nosac - vrednost support mere u pozivanju apriori-ja, po defoltu 0.05.
#     min_duzina  - minimalna duzina niza artikala, defolt 1.
#     maks_duzina - defolt 10.
#     poverenje - vrednost confidence mere u pozivanju apriori-ja, po defoltu 0.9.
#     target - defolt: "hyperedgesets"
#     sortiranje_po - prema cemo da se sortira top 20 pravila. Moguce vrednosti: "lift" (defolt), "support", "confidence".
#     sacuvaj - TRUE/FALSE da li da se pravi .xlsx fajl sa rezultatima, po defoltu - TRUE.
#     ime - naziv fajla, ako je sacuvaj=TRUE.
primeniApriori <- function(lista,  nosac = 0.05, min_duzina = 1, maks_duzina = 10, poverenje = 0.9, target = "hyperedgesets" , sortiranje_po = "lift", sacuvaj = TRUE, ime)
{
  # Pozivanje potrebnih paketa.
  suppressPackageStartupMessages(library(arules))
  suppressPackageStartupMessages(library(arulesViz))
  suppressPackageStartupMessages(library(xlsx))
  
  # Kreiranje odgovarajucih objekata - transakcije.
  data = lapply(lista, function(x) unique(x))
  data = as(data, "transactions")
  
  # Pravljenje modela i primena algoritma
  #model = apriori(data, parameter = list(support = nosac, confidence = poverenje, minlen = min_duzina, target = target))
  model = apriori(data, parameter = list(support = nosac, confidence = poverenje, minlen = min_duzina, maxlen = maks_duzina))
  
  # Top 20 prema zadatom parametru "sortiranja_po"
  print(length(model))
  if(length(model) > 1)
    inspect(head(model, n = 20, by = sortiranje_po))
  if(sacuvaj & length(model) > 1)
  {
    suppressPackageStartupMessages(library(xlsx))
    if(is.na(ime))
    {
      ime = 'Apriori'
    }
    file = paste("C:/Eksport/Rezultati/Apriori/", ime, ".xlsx", sep = "")
    res = write.xlsx(inspect(head(model, n = 10000, by = sortiranje_po)), file) 
    #quality(model)
  }
  return(model)
}

# Funkcija "primeniEclat" - primenjuje apriori algoritam na zadatu listu "lista".
#   Argumenti:
#     lista - lista podataka na koju se primenjuje apriori algoritam.
#     nosac - vrednost support mere u pozivanju apriori-ja, po defoltu 0.05.
#     min_duzina  - minimalna duzina niza artikala, defolt 1.
#     maks_duzina  - maksimalna duzina niza artikala, defolt 10.
#     target - defolt "maximally frequent itemsets"
#     sortiranje_po - prema cemo da se sortira top 20 pravila. Moguce vrednosti: (defolt)"support" itd.
#     sacuvaj - TRUE/FALSE da li da se pravi .xlsx fajl sa rezultatima, po defoltu - TRUE.
#     ime - naziv fajla, ako je sacuvaj=TRUE.
primeniEclat <- function(lista, nosac = 0.1, min_duzina = 1, maks_duzina = 10, target = "maximally frequent itemsets", sortiranje_po = "support", sacuvaj = TRUE, ime)
{
  # Pozivanje potrebnih paketa.
  suppressPackageStartupMessages(library(arules))
  suppressPackageStartupMessages(library(arulesViz))
  
  # Kreiranje odgovarajucih objekata - transakcije.
  data = lapply(lista, function(x) unique(x))
  data = as(data, "transactions")
  
  # Pravljenje modela i primena algoritma
  model = eclat(data, parameter = list(support = nosac, minlen = min_duzina, maxlen = maks_duzina, target = target))
  
  # Top 20 prema zadatom parametru "sortiranja_po"
  if(length(model) > 1)
    inspect(head(model, n = 20, by = sortiranje_po))
  if(sacuvaj & length(model) > 1)
  {
    suppressPackageStartupMessages(library(xlsx))
    if(is.na(ime))
    {
      ime = 'Eclat'
    }    
    file = paste("C:/Eksport/Rezultati/Eclat/", ime, ".xlsx", sep = "" )
    res = write.xlsx(inspect(head(model, n = 5000, by = sortiranje_po)), file) 
  }
  #pravila =  ruleInduction(model, data, confidence =0.9)
  return(model)
}


# primer primene
Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
for(i in unique(Artikli$ORGJED))
{
  baza = Artikli[which(Artikli$KOLICINA > 0 & Artikli$ORGJED == i), ]
  aggdata = by(baza[ , c("NAZIV")], INDICES = baza$RACUN_ID, FUN = paste)
  primeniApriori(aggdata,nosac = 0.001, poverenje = 0.25, ime = paste("generalno_za_sve_kupce_U_", i, sep = '') )
  primeniEclat(aggdata, nosac = 0.001, min_duzina = 1, maks_duzina = 7, ime = paste("generalno_za_sve_kupce_U_", i, sep = ''))
  
}
baza = Artikli[which(Artikli$KOLICINA > 0 & Artikli$KLASTER == 5), ]

aggdata = by(baza[ , c("NAZIV")], INDICES = baza$RACUN_ID, FUN = paste)
#aggdata = lapply(aggdata, function(x) unique(x))

primeniApriori(aggdata, minimalna_duzina = 1, ime = "test")
primeniEclat(aggdata, nosac = 0.05, min_duzina = 2, maks_duzina = 10, ime = "generalno_za_lojalne_kupce_min_2")

aggdata = by(baza[ , c("NAZIV")], INDICES = baza$RACUN_ID, FUN = paste)
#aggdata = baza
primeniApriori(aggdata, nosac = 0.0005, poverenje = 0.03, min_duzina = 2, maks_duzina = 3,  ime = "malo_poverenje")
primeniEclat(aggdata, nosac = 0.0025, min_duzina = 3, maks_duzina = 4, ime = "za klaster 5")

#aggdata = by(baza[,c("NAZIV")], INDICES = baza$KARTICALOJALNOSTI, FUN = paste)
lk = unique(Artikli$KARTICALOJALNOSTI)

for(i in na.omit(lk))
{
  baza = Artikli[which(Artikli$KOLICINA > 0 & Artikli$KARTICALOJALNOSTI == i), ]
  if(length(baza[[1]]) > 15)
  {
    aggdata = by(baza[ , c("NAZIV")], INDICES = baza$KARTICALOJALNOSTI, FUN = paste)
  
  #aggdata[which(aggdata$KARTICALOJALNOSTI == i),]
  #primeniApriori(aggdata,nosac = 0.05, poverenje = 0.8, ime = paste("generalno_za_loalnog_kupca", i, sep = "_"))
   # primeniEclat(aggdata, nosac = 0.5, maks_duzina = 7, ime = paste("generalno_za_lojalnog_kupca", i, sep = "_"))
    sapply(aggdata, function(x) primeniEclat(x, nosac = 0.01, min_duzina = 1, maks_duzina = 7, sortiranje_po = "support", 
                                             ime = paste("generalno_za_lojalnog_kupca_min2", i, sep = "_")))
    #sapply(aggdata, function(x) primeniApriori(x, nosac = 0.005, poverenje = 0.2, ime = paste("generalno_za_lojalnog_kupca", i, sep = "_")))
   # print(inspect(a[[1]]))
  }
}


###########################
## CHURN LOJALNIH KUPACA ##
###########################

# Funkcija "primeniChurn" - primenjuje Churn metode za lojalne kupce pomocu BG/NBD modela.
#   Argumenti:
#     baza - baza sa tacno 3 kolone u zadatom redosledu: lojalni kupac, datumi kad se vrsila kupovina, vrednost.
#     datum_kraja_kalibracije - datum do kog trenutka nam je test skup, defolt "2017-01-01".
#     censor - defolt 25
#     T.star - defolt 200.
primeniChurnBgNbd <- function(baza, datum_kraja_kalibracije = "2017-01-01", censor = 25, T.star = 200)
{
  suppressPackageStartupMessages(library(BTYD))
  
  colnames(baza) = c('cust','date', 'sales')
  baza = dc.MergeTransactionsOnSameDate(baza)
  
  end.of.cal.period = as.Date(datum_kraja_kalibracije)
  baza.cal = baza[which(baza$date <= end.of.cal.period), ]
  
  # Spremanje podataka i ocena parametra za BG/NBD model 
  split.data = dc.SplitUpElogForRepeatTrans(baza.cal)
  clean.baza = split.data$repeat.trans.elog
  freq.cbt = dc.CreateFreqCBT(clean.baza)
  tot.cbt = dc.CreateFreqCBT(baza.cal)
  cal.cbt = dc.MergeCustomers(tot.cbt, freq.cbt)
  birth.periods = split.data$cust.data$birth.per
  last.dates = split.data$cust.data$last.date
  cal.cbs.dates = data.frame(birth.periods, last.dates, end.of.cal.period)
  cal.cbs = dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per = "day")
  params = bgnbd.EstimateParameters(cal.cbs, par.start = c(2, 2, 2, 2), max.param.value = 150)
  print("Parametri modela:")
  print(params)
  
  elog = dc.SplitUpElogForRepeatTrans(baza)$repeat.trans.elog; 
  x.star = rep(0, nrow(cal.cbs)); 
  cal.cbs = cbind(cal.cbs, x.star); 
  baza.custs = elog$cust; 
  for (i in 1:nrow(cal.cbs))
  { 
    current.cust = rownames(cal.cbs)[i] 
    tot.cust.trans = length(which(baza.custs == current.cust)) 
    cal.trans = cal.cbs[i, "x"] 
    cal.cbs[i, "x.star"] = tot.cust.trans - cal.trans 
  } 
  # Grafik kvaliteta modela.
  x.star = cal.cbs[ , "x.star"] 
  comp = bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star, cal.cbs, x.star, censor)
  
  
  # Ispod su grafici i procene o kolicini kupovine kupaca u narednih 60 dana i verovatnoci da su zivi.
  x = cal.cbs[, "x"] 
  t.x = cal.cbs[, "t.x"] 
  T.cal = cal.cbs[, "T.cal"] 
  
  procena_kupovina = bgnbd.ConditionalExpectedTransactions(params, T.star = 60, x, t.x, T.cal)
  procena_kupovina[which(procena_kupovina > 1)]
  plot(density(procena_kupovina))
  
  procena_zivota = bgnbd.PAlive(params, x, t.x, T.cal)
  procena_zivota[which(procena_zivota < 0.5)]
  plot(density(procena_zivota))
  return(procena_zivota)
}

# Primer primene
baza_za_rad = fact_racuni[which(is.na(fact_racuni$KARTICALOJALNOSTI) == FALSE & fact_racuni$KOLICINA > 0), ]
baza_za_rad = baza_za_rad[order(baza_za_rad$DAY_DATE), ]
baza = baza_za_rad[c('KARTICALOJALNOSTI', 'DAY_DATE', 'MP_VREDNOST')]
baza$DAY_DATE = as.Date(baza$DAY_DATE, "%Y%m%d")
q = primeniChurnBgNbd(baza)
#which(q==190)
#which(names(q)=="150")
w = primeniChurnBgNbd(baza)
1 - w[which(names(w) == "160")]


###################################
### KLASTERING LOJALNIH KUPACA ####
###################################

# Funkcija "primeniKlasteringKMeans" - primenjuje KMEANS() klastering na prosledjenu bazu.
#   Argumenti:
#     baza - podaci koji se klasterizuju.
primeniKlasteringKMeans <- function(baza, broj_klastera = NA, min_broj_klastera = 4, maks_broj_klastera = 15,  siluet = FALSE, ccc = TRUE)
{
  if(is.na(broj_klastera) == TRUE & siluet)
  {
    suppressPackageStartupMessages(library(bios2mds))
    broj_klastera = sil.score(baza, nb.clus = min_broj_klastera:maks_broj_klastera)
    print(broj_klastera)
    broj_klastera = which.max(broj_klastera)
  }
  if(is.na(broj_klastera) == TRUE & ccc)
  {
    suppressPackageStartupMessages(library(NbClust))
    broj_klastera = NbClust(baza, distance = "euclidean", min.nc = min_broj_klastera, max.nc = maks_broj_klastera, method = "kmeans", index = "ccc")$Best.nc[[1]]
  }
    
  k_mins = kmeans(baza, broj_klastera)
  print(k_mins)
  plot(baza, col=k_mins$cluster)
  return(cbind(baza, k_mins$cluster)) 
}

# Primer primene:
primeniKlasteringKMeans(kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE[-1], min_broj_klastera = 4, maks_broj_klastera = 20)

primeniKlasteringKMeans(kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE[-1], min_broj_klastera = 4, maks_broj_klastera = 20, ccc = FALSE,  siluet = TRUE)

####################
# Analiza klastera #
####################

# Funkcija "analizaKlasteriLojalniKupci" - 
#   Argumenti:
#     baza - 
#     broj_klastera - 
#     nosac_apriori
#     nosac_eclat 
#     poverenje_apriori
#     ime - naziv fajlova za apriori i Eclat algoritme.
#   Napomena:
#       ucitati baze - kartice_lojalnosti, fact_racuni, artikliLatinica.
#       potrebne funkcije iz biblioteke - primeniKlasteringKMeans, primeniEclat, primeniApriori.

analizaKlasteriLojalniKupci <- function(baza, broj_klastera, nosac_apriori, nosac_eclat, poverenje_apriori, ime)
{
  primenaklastera = primeniKlasteringKMeans(baza = baza[-1], broj_klastera = broj_klastera)
  kartice_lojalnosti = merge(kartice_lojalnosti, cbind(baza[1], primenaklastera), by = 'KARTICALOJALNOSTI')
  colnames(kartice_lojalnosti)[37] = "KLASTER"
  # Sad imamo i ko je iz kog klastera.
  
  # Nas svakim klasterom ECLAT i Apriori.
  Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
  for(i in 1:broj_klastera)
  {
    message("Analiza klastera broj:", i)
    baza_nova = kartice_lojalnosti[which(kartice_lojalnosti$KLASTER == i), ]
    print('broj ZENA je')
    print(sum(baza_nova$POL == "Z"))
    print('broj MUSKIH je')
    print(sum(baza_nova$POL == "M"))
    baza_nova = merge(Artikli, baza_nova, by = "KARTICALOJALNOSTI")
    aggdata = by(baza_nova[,c("NAZIV")], INDICES = baza_nova$RACUN_ID, FUN = paste)
    primeniEclat(aggdata, nosac = nosac_eclat, ime = paste(ime, 'klaster', i, sep = "_"))
    primeniApriori(aggdata, nosac = nosac_apriori, poverenje = poverenje_apriori, ime = paste(ime, 'klaster', i, sep = "_"))
  }
}


# Primer primene
analizaKlasteriLojalniKupci(kmeans_svisemSTAROST_KUPCA, broj_klastera = 8, nosac_eclat = 0.01, nosac_apriori = 0.001,
                            poverenje_apriori = 0.9, ime = "kmeans_svisemSTAROST_KUPCA")

analizaKlasteriLojalniKupci(kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE, broj_klastera = 9, nosac_eclat = 0.01, 
                            nosac_apriori = 0.003, poverenje_apriori=0.3, ime = "kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE")



#########################
# GADJANJE LOSIH KUPACA #
#########################

# Funkcija "gadjanjeLosihKupaca" - vraca artikli koji svi kupci u churn-u najvise kupuju.
#   Argumenti:
#     baza - baza pripremljena za funkciju primeniChurnBgNbd().
#     procenat_smrti - defolt 0.5, procenat od kojeg broja racunamo da kupac ulazi u churn.
#     nosac_eclat - koliki nosac pri primeni eclat algoritma, defolt = 0.99
#     ime - 
#   Napomena:
#     ucitati - fact_racuni, artikliLatinica i napraviti bazu "Artikli" pozivom: Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
#     funkcije - primeniChurnBgNbd(), primeniEclat().
gadjanjeLosihKupaca <- function(baza, procenat_smrti = 0.5, nosac_eclat = 0.99, ime)
{
  procena_smrti = 1 - primeniChurnBgNbd(baza = baza)
  indeksi = which(procena_smrti >= procenat_smrti, arr.ind = TRUE)
  aggbaza = Artikli[ which(Artikli$KARTICALOJALNOSTI %in% names(indeksi)), ]
  aggdata = by(aggbaza[ , c("NAZIV")], INDICES = aggbaza$KARTICALOJALNOSTI, FUN = paste)

  primeniEclat(aggdata, nosac_eclat, ime = paste('losi_kupci', 'analiza', ime, sep = '_'))
}

# Funkcija "gadjanjeLosegKupca" - vraca artikle koji odredjeni kupac u churn-u najvise kupuje.
#   Argumenti:
#     baza - baza pripremljena za funkciju primeniChurnBgNbd().
#     procenat_smrti - defolt 0.5
#     nosac_eclat - koliki nosac pri primeni eclat algoritma, defolt = 0.99
#     maks_duzina -  najveca duzina n-torke u eclat-u.
#   Napomena:
#     ucitati - fact_racuni, artikliLatinica i napraviti bazu "Artikli" pozivom: Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
#     funkcije - primeniChurnBgNbd(), primeniEclat().
gadjanjeLosegKupca <- function(baza, procenat_smrti = 0.5, nosac_eclat = 0.99, maks_duzina = 10, ime)
{
  procena_smrti = 1 - primeniChurnBgNbd(baza = baza)
  indeksi = which(procena_smrti >= procenat_smrti, arr.ind = TRUE)
  indeksi = sort(indeksi)
  lista = list()
  for(i in names(indeksi))
  {
    aggbaza = Artikli[which(Artikli$KARTICALOJALNOSTI == i), ]
    aggdata = by(aggbaza[ , c("NAZIV")], INDICES = aggbaza$KARTICALOJALNOSTI, FUN = paste)
    message("LOJLANI KUPAC: ", i)
    if(length(aggbaza[[1]]) > 2) 
      lista[i] = sapply(aggdata, function(x) primeniEclat(x, nosac = nosac_eclat, maks_duzina = maks_duzina, min_duzina = 1, sacuvaj = FALSE))
  }
  for(i in names(lista))
  {
    #message(i)
    #inspect(lista[[i]])
    file = paste("C:/Eksport/Rezultati/Eclat/", ime, i, ".xlsx", sep = "")
    write.xlsx(inspect(lista[[i]], by= "support"), file = file)
  }
  return(lista)
}

# Primer primene:
baza_za_rad = fact_racuni[which(is.na(fact_racuni$KARTICALOJALNOSTI) == FALSE), ]
baza_za_rad = baza_za_rad[order(baza_za_rad$DAY_DATE), ]
baza = baza_za_rad[c('KARTICALOJALNOSTI', 'DAY_DATE', 'MP_VREDNOST')]
baza$DAY_DATE = as.Date(baza$DAY_DATE, "%Y%m%d")
Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')

q = gadjanjeLosihKupaca(baza,procenat_smrti = 0.5, nosac_eclat = 0.15, ime = "kriticne_vitalnosti")
gadjanjeLosegKupca(baza, procenat_smrti = 0.5, nosac_eclat = 0.01, maks_duzina = 10, ime = "kriticna_vitalnost")

# Ispis artikala.
for(i in names(q))
{
    message(i)
    inspect(q[[i]])
 
}


################################################
########## FUZZY C-MEANS CLUSTERING ############
################################################

primeniKlasteringCMeans <- function(baza, broj_klastera = NA, min_broj_klastera = 4, maks_broj_klastera = 15, m = 1.5, siluet = FALSE, ccc = TRUE)
{
  suppressPackageStartupMessages(library(e1071))
  if(is.na(broj_klastera) & siluet)
  {
    suppressPackageStartupMessages(library(bios2mds))
    broj_klastera = sil.score(baza,nb.clus = (min_broj_klastera:maks_broj_klastera))
    print(broj_klastera)
    broj_klastera = which.max(broj_klastera)
  }
  if(is.na(broj_klastera) & ccc)
  {
    suppressPackageStartupMessages(library(NbClust))
    broj_klastera = NbClust( baza, distance="euclidean", min.nc = min_broj_klastera, max.nc = maks_broj_klastera, method = "kmeans", index = "ccc" )$Best.nc[[1]]
  }
  c_mins = cmeans(baza, centers = broj_klastera, m = m)
  #print(c_mins)
  table(c_mins$cluster)
  plot(baza, col=c_mins$cluster)
  
  return(cbind(baza, c_mins$cluster)) 
}

# Primer primene
primeniKlasteringCMeans(kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE[-1], broj_klastera = 9, m = 1.8 )


####################
# Analiza klastera #
####################

# Funkcija "analizaKlasteriLojalniKupci" - 
#   Argumenti:
#     baza - 
#     broj_klastera - 
#     nosac_apriori
#     nosac_eclat 
#     poverenje_apriori
#   Napomena:
#       ucitati baze - kartice_lojalnosti, fact_racuni, artikliLatinica.
#       potrebne funkcije iz biblioteke - primeniKlasteringCMeans, primeniEclat, primeniApriori.
analizaFuzzyKlasteriLojalniKupci <- function(baza, broj_klastera, nosac_apriori, nosac_eclat, poverenje_apriori, m = 1.8)
{
  primenaklastera = primeniKlasteringCMeans(baza = baza[-1], broj_klastera = broj_klastera, m = m)
  kartice_lojalnosti = merge(kartice_lojalnosti, cbind(baza[1], primenaklastera), by = 'KARTICALOJALNOSTI')
  colnames(kartice_lojalnosti)[37] = "KLASTER"
  # Sad imamo i ko je iz kog klastera.
  
  # Nas svakim klasterom ECLAT i Apriori.
  Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
  
  for(i in 1:broj_klastera)
  {
    message("Analiza klastera broj:", i)
    baza_nova = kartice_lojalnosti[which(kartice_lojalnosti$KLASTER == i), ]
    print('broj ZENA je')
    print(sum(baza_nova$POL == "Z"))
    print('broj MUSKIH je')
    print(sum(baza_nova$POL == "M"))
    baza_nova = merge(Artikli, baza_nova, by = "KARTICALOJALNOSTI")
    aggdata = by(baza_nova[ ,c("NAZIV")], INDICES = baza_nova$RACUN_ID, FUN = paste)
    primeniEclat(aggdata, nosac = nosac_eclat )
    primeniApriori(aggdata, nosac = nosac_apriori, poverenje = poverenje_apriori)
  }
}


# Primer primene
analizaFuzzyKlasteriLojalniKupci(kmeans_svisemSTAROST_KUPCA, broj_klastera = 8, nosac_eclat = 0.01, nosac_apriori = 0.001, poverenje_apriori = 0.9)

analizaFuzzyKlasteriLojalniKupci(kmeans_svisemSTAROST_KUPCA_I_NAJSKORIJA_KUPOVINA_PRE, broj_klastera = 9, nosac_eclat = 0.01,
                                 nosac_apriori = 0.003, poverenje_apriori = 0.5)




#################################################################
###################### PROPUSTENE PRODAJE #######################
#################################################################


# Funkcije "propustena_prodaja" - trazi koliko novca je izgubljeno, jer nije prodat dodatan artikal iz date kategorije.
#     Argumenti:
#       Artikli - baza koja predstavlja merge-ovanu fact_racuni i artikliLatinica.
#       baza - artikli agregirane po racun_id - pomocu "by" funkcije.
#       artikal1 - prvi artikal.
#       artikal2 - drugi artikal.
#       artikal3 - artikal za koji se gleda koliko novca je izgubleno usled njegove neprodaje.
#     Napomene:
#       ucitati - fact_racuni, artikliLatinica i napraviti bazu Artikli.
propustena_prodaja <- function(Artikli, baza, artikal)
{
  pronadji_advanced <- function(baza, artikli)
  {
    duzina = length(artikli)
    artikal = vector(length = duzina)
    #return(ifelse(mean(artikli %in% baza) == 1, 1, 0))
    for(i in 1:duzina)
    {
      if(!(artikli[i] %in% baza))
        return(0)
      
      if(artikli[i] %in% baza & i < duzina)
        next
      
      if(artikli[i] %in% baza & i == duzina)
        return(1)
    }
  }
  
  kolicina1_2_3 = sum(sapply(baza, function(x) pronadji_advanced(x, artikal)))
  kolicina1_2 = sum(sapply(baza, function(x) pronadji_advanced(x, artikal[-length(artikal)])))
  message('Kolcina1_2:')
  print(kolicina1_2)
  message('Kolcina1_2_3:')
  print(kolicina1_2_3)
  
  message('Odnos:')
  odnos = kolicina1_2_3/kolicina1_2
  print(odnos)
  
  nova_baza = Artikli[which(Artikli$NAZIV == artikal[length(artikal)] & Artikli$KOLICINA > 0), ]
  
  message('Srednja vrednost jedne izgubljene prodaje:')
  srednja_vrednost = mean(nova_baza[ ,'MP_CENA'])
  print(srednja_vrednost)
  
  message('Ukupno izgubljeno:')
  return(odnos * (kolicina1_2 - kolicina1_2_3) * srednja_vrednost)
}

# Funkcije "propustena_prodaja_sve" - trazi koliko novca je izgubljeno, jer nije prodat dodatan artikal iz date kategorije.
#     Argumenti:
#       Artikli - baza koja predstavlja merge-ovanu fact_racuni i artikliLatinica.
#       baza - artikli agregirane po racun_id - pomocu "by" funkcije.
#       lista_artikala - lista gde je svaki element grupa artikala
#       biraj_mimimalno - TRUE\FALSE promenljiva koja kaze da li da od zadatog skupa odabere prosecno najjeftiniji ili ne. Defolt: TRUE.
#       ime - naziv fajla koji se napravi nakon izvrsenja         
#     Napomene:
#       ucitati - fact_racuni, artikliLatinica i napraviti bazu Artikli.
propustena_prodaja_sve <- function(Artikli, baza, lista_artikal, biraj_minimalno = TRUE, ime)
{
  matrica_propustena_prodaja = matrix(nrow = length(lista_artikal), ncol = 5)
  colnames(matrica_propustena_prodaja) = c('Kupljeno', 'Dodatak', 'Srednja vrednost dodatka', 'Odnos', 'Izgubljeno')
  pronadji_advanced <- function(baza, artikli)
  {
    duzina = length(artikli)
    artikal = vector(length = duzina)
    for(i in 1:duzina)
    {
      if( !(artikli[i] %in% baza))
        return(0)
      
      if(artikli[i] %in% baza & i < duzina)
        next
      
      if(artikli[i] %in% baza & i == duzina)
        return(1)
    }
  }
  
  for(i in 1:length(lista_artikal))
  {
    artikal = lista_artikal[[i]]
    
    minimalni = length(artikal)
    sr_vr = 10000000000
    if(biraj_minimalno)
    {
      for(j in 1:length(artikal))
      {
        nova_baza = Artikli[which(Artikli$NAZIV == artikal[j] & Artikli$KOLICINA > 0), ]
        srednja_vrednost = mean(nova_baza[ ,'MP_CENA'])
        if(is.na(srednja_vrednost) == FALSE )
        {
          if(srednja_vrednost < sr_vr )
          {
            sr_vr = srednja_vrednost
            minimalni = j
          }
        }
      }
    }
    kolicina1_2_3 = sum(sapply(baza, function(x) pronadji_advanced(x, artikal[minimalni])))
    kolicina1_2 = sum(sapply(baza, function(x) pronadji_advanced(x, artikal[-minimalni])))
    
    matrica_propustena_prodaja[i, 1] = paste(artikal[-minimalni][1], artikal[-minimalni][2], artikal[-minimalni][3], artikal[-minimalni][4] )
    
    matrica_propustena_prodaja[i, 2] = artikal[minimalni]
    
    matrica_propustena_prodaja[i, 4] = round(1000*kolicina1_2_3/kolicina1_2)/1000
    
    nova_baza = Artikli[which(Artikli$NAZIV == artikal[minimalni] & Artikli$KOLICINA > 0), ]
    matrica_propustena_prodaja[i, 3] = round(10*mean(nova_baza[ , 'MP_CENA']))/10
    
    matrica_propustena_prodaja[i, 5] = round(10*kolicina1_2_3/kolicina1_2 * (kolicina1_2 - kolicina1_2_3) * mean(nova_baza[ ,'MP_CENA']))/10
  }
  suppressPackageStartupMessages(library(xlsx))
  write.xlsx(matrica_propustena_prodaja, file = paste("C:/Eksport/Rezultati/Propustena_prodaja/propustene_prodaje_", ime, ".xlsx", sep = "") )
  return(matrica_propustena_prodaja)
}


## Primer primene "propustena_prodaja_sve":
library(readr)
fact_racuni <- read_csv("C:/Eksport/fact_racuni.csv", col_types = cols(DAY_DATE = col_date(format = "%d.%m.%Y"),HOUR_DATE = col_datetime(format = "%d.%m.%Y %H:%M:%S")))
artikliLatinica <- read_csv("~/Downloads/artikliLatinica.csv")
Artikli = merge(fact_racuni, artikliLatinica, by = 'ARTIKAL')
Artikli = merge(Artikli, Rezultat_opsti_klastering, by = 'KARTICALOJALNOSTI')

Artikli1 = Artikli[which(Artikli$KLASTER == 0 & Artikli$KOLICINA > 0), ]
baza = by(Artikli[which(as.numeric(substr(Artikli$DAY_DATE, 1, 4) ) >= 2009 ), ]$NAZIV, INDICES = Artikli[which(as.numeric(substr(Artikli$DAY_DATE, 1, 4) ) >= 2009 ), ]$RACUN_ID, FUN = paste)

#sum(ifelse(Artikli1$NAZIV == "zh.chevli", Artikli1$MP_VREDNOST, 0 ) )  + sum(ifelse(Artikli1$NAZIV == "chevli", Artikli1$MP_VREDNOST, 0 ) ) 


library(readxl)
bla_bla <- read_excel("C:/Eksport/Rezultati/Eclat/bla-bla.xlsx")

funkcija <- function(x)
{
  x=as.character(x)
  return((substr(x, 2, nchar(x)-1)) )
}

bla_bla[2] = sapply(bla_bla[[2]], function(x) funkcija(x))
lista_artikala = list()
for(i in 1:369)
{
  lista_artikala[[i]] = c(unlist(strsplit(as.character(bla_bla[i,2]), split = "[,]" )))
}
lista_artikala[1:10]
propustena_prodaja_sve(Artikli, baza, lista_artikala, TRUE, ime = "eclat_top_369_od_2009godine_original")
propustena_prodaja_sve(Artikli, baza, lista_artikala, FALSE, ime = "eclat_top_369_od_2009godine_bez_odabira")


# I za "propustena_prodaja":
propustena_prodaja(Artikli, baza, c('zh.pantaloni', 'm.pantaloni'))
propustena_prodaja(Artikli, baza, c('m.pantaloni', 'zh.pantaloni'))
propustena_prodaja(Artikli, baza, c('kiloti', 'maica'))
propustena_prodaja(Artikli, baza, c('m.pantaloni', 'zh.maica'))
propustena_prodaja(Artikli, baza, c('kutija za poklon','maica', 'pantaloni'))

propustena_prodaja(Artikli, baza, c('kutija za poklon','pantaloni', 'maica' ))





propustena_prodaja(Artikli, baza, c('koshula', 'pantaloni', 'remen'))

propustena_prodaja(Artikli, baza, c('m.maica', 'kutija za poklon', 'zh.maica'))
propustena_prodaja(Artikli, baza, c('zh.maica', 'kutija za poklon', 'm.maica'))
propustena_prodaja(Artikli, baza, c('zh.maica', 'm.maica', 'kutija za poklon'))


#propustena_prodaja(Artikli, baza, c('m. maica', 'kutija za poklon', 'zh.maica'))
#propustena_prodaja(Artikli, baza, c('zh.maica', 'kutija za poklon', 'm. maica'))

#propustena_prodaja(Artikli, baza, c('m. maica', 'kutija za poklon', 'zh. maica'))
#propustena_prodaja(Artikli, baza, c('zh. maica', 'kutija za poklon', 'm. maica'))


#propustena_prodaja(Artikli, baza, c('m.maica', 'kutija za poklon', 'zh. maica'))
#propustena_prodaja(Artikli, baza, c('zh. maica', 'kutija za poklon', 'm.maica'))



#propustena_prodaja(Artikli, baza, c('zh.maica', 'kutija za podarok', 'm.maica'))
#propustena_prodaja(Artikli, baza, c('m.maica', 'kutija za podarok', 'zh.maica'))

#propustena_prodaja(Artikli, baza, c('m. maica', 'kutija za podarok', 'zh.maica'))
#propustena_prodaja(Artikli, baza, c('zh.maica', 'kutija za podarok', 'm. maica'))

#propustena_prodaja(Artikli, baza, c('m. maica', 'kutija za podarok', 'zh. maica'))
#propustena_prodaja(Artikli, baza, c('zh. maica', 'kutija za podarok', 'm. maica'))

#propustena_prodaja(Artikli, baza, c('m.maica', 'kutija za podarok', 'zh. maica'))
#propustena_prodaja(Artikli, baza, c('zh. maica', 'kutija za podarok', 'm.maica'))

for(i in unique(Artikli$KORISNIK)[1:5])
{
  Artikli1 = Artikli[which(Artikli$KORISNIK == i & Artikli$KOLICINA > 0), ]
  baza = by(Artikli1[, c('NAZIV')], INDICES = Artikli1$RACUN_ID, FUN = paste)
  propustena_prodaja_sve(Artikli, baza, lista_artikala, ime = paste("korisnika_", i, sep = '') )
}


