
setwd('C:/Dados/GitHub/CheckNamesBrazilianFlora2020')
source('CheckNamesBrazilianFlora2020.R')


#confere listagem

spp <- fread(choose.files(), na.strings="", encoding="Latin-1") # encoding="UTF-8" and "Latin-1"

spp$g <- spp$genus
spp$s <- spp$specificEpithet
spp$i <- spp$infraspecificEpithet

spp$g <- ifelse(is.na(spp$g),'',spp$g)
spp$g <- ifelse(spp$g=='',spp$family,spp$g)
spp$s <- ifelse(is.na(spp$s),'',spp$s)
spp$i <- ifelse(is.na(spp$i),'',spp$i)
spp$spp <- paste0(spp$g,' ',spp$s,' ',spp$i)

flora <- get.taxa(spp$spp[1])[0,]
sis <- confere.lista.FloraBR2020(spp$g[1],spp$s[1],spp$i[1])[0,]
hab <- habito.FloraBR2020(1)[0,]
sub <- substrato.FloraBR2020(1)[0,]
uf <- distribuicao.uf.FloraBR2020(1)[0,]

for(l in 1:NROW(spp))
{
  
  flora.tmp <- get.taxa(spp$spp[l])
  sis.tmp <- confere.lista.FloraBR2020(spp$g[l],spp$s[l],spp$i[l],maxDist=2)
   
  id <- sis.tmp$id # id <- nome.aceito.FloraBR2020(spp$g[l],spp$s[l],spp$i[l],maxDist=2)$id
  hab.tmp <- habito.FloraBR2020(id)[2,]
  sub.tmp <- substrato.FloraBR2020(id)[1,]
  uf.tmp <- distribuicao.uf.FloraBR2020(id)[1,]
  
  flora <- rbind(flora,flora.tmp)
  sis <- rbind(sis,sis.tmp)
  hab <- rbind(hab,hab.tmp)
  sub <- rbind(sub,sub.tmp)
  uf <- rbind(uf,uf.tmp)
  print(paste0(l,' - ', sis.tmp$status,' [', sis.tmp$scientificNamewithoutAuthor,'] <- (',spp$g[l], ' ', spp$s[l], ') ',flora.tmp$notes))
}

nome.busca <- data.frame(gen.busca=spp$g, esp.busca=spp$s, inf.esp.busca=spp$i)
suplementar <- cbind(nome.busca ,sis, hab,sub,uf, flora)

fwrite(suplementar,file.choose())



# confere sinônimos

spp <- fread(choose.files(), na.strings="")
spp.check <- confere.lista.FloraBR2020(spp$g,spp$s,spp$i)
fwrite(spp.check,file.choose())



spp <- fread(choose.files(), na.strings="")





limpaNA<-function(x)
{
  if(is.na(x)){x<-''}
  x<-gsub('NA','',x)
  return(x)}
