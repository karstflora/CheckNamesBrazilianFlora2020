### Pesquisa por nomes aceitos e sinônomos conforme Projeto Flora do Brasil 2020  ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###   CheckNamesBrazilianFlora2020	###

###---------------------------------------------------------------------###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, raster)

###---------------------------------------------------------------------###

# instruções

# acessar http://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil
# download the latest version of this resource data as a Darwin Core Archive (DwC-A)
# descompactar o arquivo e salvar (taxon.txt) no diretório de trabalho ou indicar caminho

###---------------------------------------------------------------------###

taxon.full <- fread("T:/GitHub/CheckNamesBrazilianFlora2020/taxon.txt")

t <- taxon.full$taxonRank %in% c("ESPECIE","VARIEDADE","SUB_ESPECIE")
taxon <- taxon.full[t,]

g <- taxon.full$taxonRank %in% c("GENERO")
genus <- taxon.full[g,]

f <- taxon.full$taxonRank %in% c("FAMILIA")
family <- taxon.full[f,]

###---------------------------------------------------------------------###

nome.aceito.FloraBR2020<-function(g='',s='',i='')
{
  
  nameNA <-data.frame(
    scientificName = "",
    family = "",
    genus = "",
    specificEpithet = "",
    infraspecificEpithet = "",
    taxonRank = "",
    scientificNameAuthorship = "",
    taxonomicStatus = "",
    nomenclaturalStatus = "",
    higherClassification = "",
    acceptedNameUsage = "",
    status = 'não encontrado',    
    scientificNamewithoutAuthor = "",
    stringsAsFactors = F )
  
  s <- gsub('sp.',"",s )
  s <- gsub("[0:9]","",s )
  s <- gsub('cf.',"",s )
  s <- gsub('aff.',"",s )
  
  s <- gsub("?","",s )
  
  i <- gsub('subsp.',"",i )
  i <- gsub('var.',"",i )
  
  g <- trim(g)
  s <- trim(s)
  i <- trim(i)
  
  name<-{}
  status <-""
  if(g!=""){
    
    if (g!=""&s!=""){
      c <- g == taxon$genus &
        s == taxon$specificEpithet &
        i == taxon$infraspecificEpithet
      name <- taxon[c,c("scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
    }
    
    if (g!=""&s==""){
      c <- g == genus$genus & genus$taxonomicStatus == 'NOME_ACEITO' & genus$nomenclaturalStatus == "NOME_CORRETO"
      name <- genus[c,c("scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
    }
    
    
    if (g!=""&s==""&!NROW(name)>=1){
      c <- g == family$family
      name <- family[c,c("scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
    }
    
    if (!NROW(name)>0){name<-nameNA}
    else {
      #print(paste0(name$scientificName," ",name$nomenclaturalStatus))
      if( name$taxonomicStatus != 'NOME_ACEITO' ){
        status <- 'ATUALIZADO'
        nome.valido <-  name$acceptedNameUsage == taxon$scientificName & taxon$taxonomicStatus == 'NOME_ACEITO' #& taxon$nomenclaturalStatus == "NOME_CORRETO"
        name <- taxon[nome.valido,c("scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
      }
      if (status==""){status <-  "OK"}
      
      name$status[1] <- status
      name$scientificNamewithoutAuthor[1] <- ""
      
      if(name$taxonRank=="ESPECIE") {name$scientificNamewithoutAuthor[1] <-
        paste0(name$genus[1]," ",
               name$specificEpithet[1])}
      

      if(name$taxonRank=="VARIEDADE") {name$scientificNamewithoutAuthor[1] <-
        paste0(name$genus[1]," ",
               name$specificEpithet[1]," var. ",
               name$infraspecificEpithet[1])}
      
      if(name$taxonRank=="SUB_ESPECIE") {name$scientificNamewithoutAuthor[1] <-
        paste0(name$genus[1]," ",
               name$specificEpithet[1]," subsp. ",
               name$infraspecificEpithet[1])}
      
      if(name$taxonRank=="GENERO") {name$scientificNamewithoutAuthor[1] <- name$genus[1]}
      
      if(name$taxonRank=="FAMILIA") {name$scientificNamewithoutAuthor[1] <- name$family[1]}
      
      
    } 
  }else {
    name<-nameNA
    name$status[1] <- "VAZIO"
    name$scientificNamewithoutAuthor[1] <-""}
  
  if(is.na(name$status[1])){name$status[1] <- "sinônimo de sinônimo"
  name$scientificNamewithoutAuthor[1] <-""}
  
  print(name$status[1])  
  
  return(name[1,])
}

###---------------------------------------------------------------------###

sinonimos.FloraBR2020<-function(g='',s='',i='')
{
  
  s <- gsub('sp.',"",s )
  s <- gsub("[0:9]","",s )
  s <- gsub('cf.',"",s )
  s <- gsub('aff.',"",s )
  
  s <- gsub("?","",s )
  
  i <- gsub('subsp.',"",i )
  i <- gsub('var.',"",i )
  
  g <- trim(g)
  s <- trim(s)
  i <- trim(i)
  
  name<-{}
  if(g!=""){
    
    if (g!=""&s!=""){
      c <- g == taxon$genus &
      s == taxon$specificEpithet &
      i == taxon$infraspecificEpithet
      name <- taxon[c,c("id","parentNameUsageID","scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
    }
    
    if (!NROW(name)>0){return(NA)}
    else {
      if( name$taxonomicStatus != 'NOME_ACEITO' ){
        nome.valido <-  name$acceptedNameUsage == taxon$scientificName & taxon$taxonomicStatus == 'NOME_ACEITO' #& taxon$nomenclaturalStatus == "NOME_CORRETO"
        name <- taxon[nome.valido,c("id","parentNameUsageID","scientificName","family","genus","specificEpithet","infraspecificEpithet","taxonRank","scientificNameAuthorship","taxonomicStatus","nomenclaturalStatus","higherClassification","acceptedNameUsage")]
      }
      
    } 
  }else {return(NA)}
  
  # sinonimo do sinomimo
  if(is.na(name$id)){return(NA)}
  
  synonym <- taxon[taxon$acceptedNameUsageID == name$id,]
  
  if(is.na(synonym$id[1])){return(NA)}
  
  s<- {}
  s <- synonym$taxonRank=="VARIEDADE"
  synonym$synonymWithoutAuthor[s] <-
    paste0(synonym$genus[s]," ",
           synonym$specificEpithet[s]," var. ",
           synonym$infraspecificEpithet[s])
  
  s<- {}
  s <- synonym$taxonRank=="SUB_ESPECIE"
  synonym$synonymWithoutAuthor[s] <-
    paste0(synonym$genus[s]," ",
           synonym$specificEpithet[s]," subsp. ",
           synonym$infraspecificEpithet[s])
  
  s<- {}
  s <- synonym$taxonRank=="ESPECIE"
  synonym$synonymWithoutAuthor[s] <-
    paste0(synonym$genus[s]," ",
           synonym$specificEpithet[s])
  
  
  return(synonym)
}

###---------------------------------------------------------------------###

# Exemplos:
 
View(nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i=''))
nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i='')$scientificNamewithoutAuthor

View(sinonimos.FloraBR2020(g='Bomarea',s='edulis',i=''))
sinonimos.FloraBR2020(g='Bomarea',s='edulis',i='')$synonymWithoutAuthor

###---------------------------------------------------------------------###
  