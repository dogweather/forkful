---
title:                "Trabalhando com CSV"
html_title:           "Haskell: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

##Qual a Utilidade do CSV

Trabalhar com CSV (valores separados por vírgulas) é um processo muito comum em programação, especialmente quando precisamos lidar com grandes quantidades de dados. CSV é um formato de arquivo simples que permite armazenar dados em colunas e linhas, facilitando sua importação e exportação por meio de planilhas eletrônicas. Programadores usam CSV por sua facilidade de uso e eficiência em manipulação de dados.


##Como Fazer:
Existe uma biblioteca chamada "haskell-csv" que nos permite trabalhar com CSV em Haskell de maneira fácil e eficiente. Vamos dar uma olhada em como usá-la:

Primeiro, precisamos importar o módulo ```Text.CSV``` para termos acesso às funções da biblioteca. Em seguida, podemos criar uma tabela CSV com a função ```parseCSV```, passando uma string contendo os valores separados por vírgulas:

```haskell
import Text.CSV    

main :: IO()    
main = do    
  let csv = "1,2,3\n4,5,6\n7,8,9"    
  let table = parseCSV csv    
  print table
```

O resultado será uma tabela contendo cada linha e coluna como uma lista de valores:

```haskell
Right [["1","2","3"],["4","5","6"],["7","8","9"]]
```

Podemos também usar a função ```printCSV``` para exibir a tabela em formato CSV novamente:

```haskell
main = do    
  let csv = "1,2,3\n4,5,6\n7,8,9"    
  let table = parseCSV csv    
  printCSV table
```

O resultado será a mesma string de CSV que inserimos anteriormente:

```haskell
1,2,3
4,5,6
7,8,9
```

A biblioteca também nos permite acessar e modificar valores específicos na tabela, bem como adicionar novas linhas e colunas. Para mais detalhes, consulte a documentação oficial da biblioteca.


##Aprofundando:
CSV é um formato de arquivo bastante utilizado na indústria de banco de dados, com sua origem datando das primeiras décadas da computação. Alternativas mais modernas, como JSON e XML, vêm ganhando popularidade para armazenar e transmitir dados, mas CSV ainda é amplamente utilizado devido à sua simplicidade e suporte universal em planilhas eletrônicas.

Em termos de implementação, a biblioteca "haskell-csv" usa o conceito de "parsing", que é a transformação de uma string em uma estrutura de dados mais complexa. Ela usa técnicas avançadas e eficientes de parsing para garantir um desempenho rápido e seguro.

Além disso, a biblioteca é bem documentada e possui código fonte aberto, o que permite aos programadores contribuírem com melhorias e correções de bugs.