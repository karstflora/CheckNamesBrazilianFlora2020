# instructoin 

### acess http://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil
### download the latest version of this resource data as a Darwin Core Archive (DwC-A)
### unzip file and set path 

# Examples
 
# View(nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i=''))

# nome.aceito.sinonimos.FloraBR2020('Bomarea','edulis')$names

# nome.aceito.sinonimos.FloraBR2020('Adiantum','calcareum')$names

# nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i='')$scientificNamewithoutAuthor

# nome.aceito.FloraBR2020(g='Hemionitis',s='sp.10',i='')$scientificNamewithoutAuthor

# View(sinonimos.FloraBR2020(g='Bomarea',s='edulis',i=''))

# sinonimos.FloraBR2020(g='Bomarea',s='edulis',i='')$synonymWithoutAuthor

# habito.FloraBR2020(nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i='')$id)

# substrato.FloraBR2020(nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i='')$id)

# distribuicao.uf.FloraBR2020(nome.aceito.FloraBR2020(g='Cavanillesia',s='arborea',i='')$id)