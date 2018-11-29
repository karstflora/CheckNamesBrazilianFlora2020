# instructoin 

### acess http://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil
### download the latest version of this resource data as a Darwin Core Archive (DwC-A)
### unzip file and set path 

# Examples

# genus <- "Cereus"
# specificEpithet <- "jamacaru"
# infraspecificEpithet <- ""
# x<-nome.aceito.FloraBR2020(genus, specificEpithet, infraspecificEpithet)

# Use the result "Does not occur in Brazil OR Unknown distribution", as an inicative, it must be checked manually on the website. The available information does not allow 100% correct response in these cases. The other results are reliable.


# habito.FloraBR2020(x$id)
 
# habito.FloraBR2020(x$id, "T")

# substrato.FloraBR2020(x$id)

# substrato.FloraBR2020(x$id, "T")

# distribuicao.uf.FloraBR2020(x$id)

# endemismo.Brasil.FloraBR2020(x$id)

# dominiofitogeografico.FloraBR2020(x$id)

# tipovegetacao.FloraBR2020(x$id)
