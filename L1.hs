--Alípio Fernando de Paula Pires - 202100022668

import Prelude
import Data.Char

type CadastroSUS = [Cidadao]
type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)
type CadastroLoc = [(Int , Cidadao)]
--(07751806532, "Alipio",'m', (17,09,2002),"rua","estancia","sergipe", "79996760917", "alipiomeueu" )
banco :: [Cidadao]
banco = [(54124706532, "Jota",'M', (17,09,2021),"a","Olindina","Bahia", "79996760917", "alissiisu" ),
         (01141212433, "Sabrina",'X', (17,01,2005),"b","aracaju","sergipe", "79996760917", "alipiomeueu" ),
         (04565163738, "Amelia",'F', (12,03,1996),"c","aracaju","sergipe", "79996760917", "alipiomeueu" ),
         (02352357752, "Joao Carlos",'M', (05,11,2007),"d","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (07124567833, "Victor",'M', (26,04,2002),"e","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (01245452346, "Larry",'X', (10,12,1965),"f","Salvador","Bahia", "79996760917", "lazluuuuu" ),
         (07751806532, "Alipio",'M', (17,09,2002),"rua","estancia","sergipe", "79996760917", "alipiomeueu" )
        ]


adicionaSUS :: Cidadao -> CadastroSUS
adicionaSUS idadao
                | not (gendercheck idadao) = error "Por favor, escolha um genero dentre as opçoes: 'M', 'F' ou 'X'"
                | datavalida  = error "data de nascimento invalida"
                | checaCPF (filtrocpf idadao)banco =error "Voce já está cadastrado no sistema"
                | otherwise = [head (idadao:banco)]
                  where gendercheck :: Cidadao -> Bool
                        gendercheck idadao = (filtrogen idadao == 'M') || (filtrogen idadao == 'F') || (filtrogen idadao == 'X')
                        nasccid = filtroDN(idadao)
                        diacid = filtrodia nasccid
                        mescid = filtromes nasccid
                        anocid = filtroano nasccid
                        dianow = 28
                        mesnow = 09
                        anonow = 2021
                        filtrodia :: DataNasc -> Int
                        filtrodia (x,_,_) = x
                        filtromes :: DataNasc -> Int
                        filtromes (_,x,_) = x
                        filtroano :: DataNasc -> Int
                        filtroano (_,_,x) = x
                        naonasceu = (anocid>anonow) || ((anocid==anonow) && ((mescid>mesnow) || ((mescid==mesnow) && (diacid>dianow))) )
                        datavalida :: Bool
                        datavalida = naonasceu || mescid>12 || diacid>31 || mescid<1 ||diacid<1 || anocid<1

getcpf :: CadastroSUS -> [CPF]
getcpf cadastro = [filtrocpf (cadastro !! x) | x <- [0..tamanho]]
                      where tamanho = length banco -1

filtrocpf :: Cidadao -> CPF
filtrocpf (x,_,_,_,_,_,_,_,_) = x

filtrogen :: Cidadao -> Genero
filtrogen (_,_,x,_,_,_,_,_,_) = x

filtroend :: Cidadao -> Endereco
filtroend (_,_,_,_,x,_,_,_,_) = x

filtroDN :: Cidadao -> DataNasc
filtroDN (_,_,_,x,_,_,_,_,_) = x

filtroMun :: Cidadao -> Municipio
filtroMun (_,_,_,_,_,x,_,_,_) = x

filtroUf :: Cidadao -> Estado
filtroUf (_,_,_,_,_,_,x,_,_) = x

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF cpf cadastro
  | or [cpf == cpfs | cpfs <- getcpf banco] = True
  | otherwise = False

ordena:: CadastroLoc
ordena = zip [0.. length banco] banco

getpos :: CPF -> Int
getpos cpf
 | checaCPF cpf banco = head [pos | (pos,cidadao) <- ordena , filtrocpf cidadao == cpf]
 | otherwise = 0

getcid :: CPF -> Cidadao
getcid cpf = head [cidadao | (pos,cidadao) <- ordena , filtrocpf cidadao == cpf]

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpf bnk end
 | naonulo = comeco ++ [att] ++ final
 | otherwise = error "cpf invalido"
  where pos = (getpos cpf) +1
        naonulo = pos /=0 || pos > length bnk
        comeco =  take (pos -1)(bnk)
        final = drop(pos)(bnk)
        att = atua (bnk!!(pos-1))
        atua :: Cidadao -> Cidadao
        atua (x,y,z,w,e,k,j,l,o) = (x,y,z,w,end,k,j,l,o)

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS cpf bnk tel
 | naonulo = comeco ++ [att] ++ final
 | otherwise = error "cpf invalido"
  where pos = (getpos cpf) +1
        naonulo = pos /=0 || pos > length bnk
        comeco =  take (pos -1)(bnk)
        final = drop(pos)(bnk)
        att = atua (bnk!!(pos-1))
        atua :: Cidadao -> Cidadao
        atua (x,y,z,w,e,k,j,l,o) = (x,y,z,w,e,k,j,tel,o)

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpf bnk
 | checaCPF cpf bnk = comeco++final
 | otherwise = error "cpf invalido"
   where pos = (getpos cpf) +1
         comeco =  take (pos -1)(bnk)
         final = drop(pos)(bnk)


faixas :: [FaixaIdade]
faixas = [(81,130),
          (71,80),
          (61,70),
          (51,60),
          (41,50),
          (31,40),
          (21,30),
          (12,21),
          (0, 12)
         ]
cadastrofaixa :: [(FaixaIdade, CadastroSUS)]
cadastrofaixa = [f1]++[f2]++[f3]++[f4]++[f5]++[f6]++[f7]++[f8]
   where f1 = (faixas!!0,pegaporidade banco (faixas!!0))
         f2 = (faixas!!1,pegaporidade banco (faixas!!1))
         f3 = (faixas!!2,pegaporidade banco (faixas!!2))
         f4 = (faixas!!3,pegaporidade banco (faixas!!3))
         f5 = (faixas!!4,pegaporidade banco (faixas!!4))
         f6 = (faixas!!5,pegaporidade banco (faixas!!5))
         f7 = (faixas!!6,pegaporidade banco (faixas!!6))
         f8 = (faixas!!7,pegaporidade banco (faixas!!7))
         
quantidadefaixa :: [(FaixaIdade, Quantidade)]
quantidadefaixa = [f1]++[f2]++[f3]++[f4]++[f5]++[f6]++[f7]++[f8]
   where f1 = (faixas!!0,length (pegaporidade banco (faixas!!0)))
         f2 = (faixas!!1,length (pegaporidade banco (faixas!!1)))
         f3 = (faixas!!2,length (pegaporidade banco (faixas!!2)))
         f4 = (faixas!!3,length (pegaporidade banco (faixas!!3)))
         f5 = (faixas!!4,length (pegaporidade banco (faixas!!4)))
         f6 = (faixas!!5,length (pegaporidade banco (faixas!!5)))
         f7 = (faixas!!6,length (pegaporidade banco (faixas!!6)))
         f8 = (faixas!!7,length (pegaporidade banco (faixas!!7)))     
         
pegaporidade :: CadastroSUS -> FaixaIdade -> CadastroSUS
pegaporidade bnk (x,y) = [cid | cid<- bnk, (((idade (filtrocpf cid))>=x) && ((idade (filtrocpf cid))<= y))]
         
pegapormun :: CadastroSUS -> Municipio -> CadastroSUS
pegapormun bnk mun = [cid | cid<- bnk, filtroMun cid == mun]
         
pegaporuf :: CadastroSUS -> Municipio -> CadastroSUS
pegaporuf bnk uf = [cid | cid<- bnk, filtroUf cid == uf]
         
type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int

idade :: CPF -> Int
idade cpf
 | niverpassou = anonow - anocid
 | otherwise = (anonow - anocid) - 1
  where diacid = filtrodia nasccid
        mescid = filtromes nasccid
        anocid = filtroano nasccid
        dianow = 28
        mesnow = 09
        anonow = 2021
        nasccid = filtroDN(getcid cpf)
        filtrodia :: DataNasc -> Int
        filtrodia (x,_,_) = x
        filtromes :: DataNasc -> Int
        filtromes (_,x,_) = x
        filtroano :: DataNasc -> Int
        filtroano (_,_,x) = x
        niverpassou = ((diacid<=dianow)&&(mescid==mesnow) || (mescid<mesnow))

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio bnk mun = length [qnt | qnt <- banco , filtroMun qnt == mun]

cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
cidadaosPorEstado bnk uf = length [qnt | qnt <- banco , filtroUf qnt == uf]

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade bnk mun fxa = length (pegaporidade (pegapormun bnk mun) fxa)

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade ->Quantidade
cidadaosPorEstadoIdade bnk uf fxa = length (pegaporidade (pegaporuf bnk uf) fxa)

listaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> IO()
listaMunicipioFaixas bnk mun fxas = putStr (show dados)
 where dados = geraListaMunicipioFaixas bnk mun fxas

--listaEstadoFaixas :: CadastroSUS -> Estado-> [FaixaIdade] -> IO()


geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] ->[(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas bnk mun fxas = zip fxas [f bnk mun fai | fai <- fxas]
 where f = cidadaosPorMunicipioIdade

geraListaEstadoFaixas :: CadastroSUS -> Estado -> [FaixaIdade] ->[(FaixaIdade, Quantidade)]
geraListaEstadoFaixas bnk uf fxas = zip fxas [f bnk uf fai | fai <- fxas]
 where f = cidadaosPorEstadoIdade


type QuantidadeFormatada = String
formataQuant :: Quantidade -> QuantidadeFormatada
formataQuant num = "                                        "++(show num)