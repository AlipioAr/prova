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
banco = [(54124706532, "Jota",'m', (17,09,2022),"a","estancia","sergipe", "79996760917", "alissiisu" ),
         (01141212433, "Sabrina",'f', (17,01,2005),"b","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (04565163738, "Amelia",'f', (12,03,1996),"c","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (02352357752, "Joao Carlos",'m', (05,11,2007),"d","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (07124567833, "Victor",'m', (26,04,2002),"e","estancia","sergipe", "79996760917", "alipiomeueu" ),
         (01245452346, "Larry",'m', (10,12,1965),"f","estancia","sergipe", "79996760917", "lazluuuuu" )
        ]


adicionaSUS :: Cidadao -> CadastroSUS
adicionaSUS idadao
                | checaCPF (filtrocpf idadao)banco =error "Voce já está cadastrado no sistema"
                | otherwise = [head (idadao:banco)]

getcpf :: CadastroSUS -> [CPF]
getcpf cadastro = [filtrocpf (cadastro !! x) | x <- [0..tamanho]]
                      where tamanho = length banco -1

filtrocpf :: Cidadao -> CPF
filtrocpf (x,_,_,_,_,_,_,_,_) = x

filtroend :: Cidadao -> Endereco
filtroend (_,_,_,_,x,_,_,_,_) = x

filtroDN :: Cidadao -> DataNasc
filtroDN (_,_,_,x,_,_,_,_,_) = x

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






type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int
idade :: CPF -> Int
idade cpf
 | (diacid<=dianow)&&(mescid<=mesnow)&&(anocid< anonow) = anonow - anocid
 | anocid>anonow = error "cidadao nao nasceu ainda"
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
--cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
--cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
--cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
--cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade ->Quantidade

