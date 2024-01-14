---
title:                "Elm: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com arquivos CSV no Elm?

O Elm é uma linguagem de programação funcional que tem se tornado cada vez mais popular para o desenvolvimento de aplicações web. Uma das tarefas mais comuns em projetos web é a manipulação de arquivos CSV (Comma Separated Values), que são utilizados para armazenar dados em formato de tabela. Neste artigo, vamos explorar como o Elm pode ser uma ferramenta poderosa para trabalhar com CSV.

## Como trabalhar com CSV no Elm

Para começar, é importante entender que o Elm tem uma biblioteca nativa para lidar com arquivos CSV. Isso significa que não é necessário instalar nenhum pacote adicional ou dependência para começar a trabalhar com esses arquivos.

O primeiro passo é importar a biblioteca com o comando `import` e nomear um módulo para o trabalho com CSV: 

```elm
import Csv
funcionario : Csv.Csv.Parser funcionario
```

Agora, vamos criar um arquivo CSV simples com alguns dados de funcionários:

```elm
NOME; CARGO; SALÁRIO
João; Desenvolvedor; 8000
Maria; Designer; 6000
Pedro; Analista; 7000
```

Para ler esse arquivo em código Elm, podemos utilizar a função `parse` da biblioteca Csv:

```elm
funcionarios : List funcionario
tabelaFuncionarios = Csv.parse empleado "funcionarios.csv"
```

Isso nos permitirá armazenar os dados do arquivo CSV na variável "tabelaFuncionarios", que será do tipo `List funcionario`. A partir daí, podemos manipular esses dados como quisermos, por exemplo, filtrando funcionários por salário ou cargo.

## Aprofundando no trabalho com CSV no Elm

Existem algumas outras funcionalidades úteis para trabalhar com CSV no Elm, como a possibilidade de definir o separador de valores (padrão é ","), lidar com linhas de cabeçalho e formatação de valores de acordo com tipos de dados.

Além disso, a biblioteca Csv suporta a leitura de arquivos remotamente, o que significa que podemos fornecer uma URL em vez de um arquivo local para leitura de dados CSV.

É importante lembrar que, ao trabalhar com CSV no Elm, é necessário ficar atento ao formato dos dados e garantir que eles estejam consistentes para que a leitura seja bem-sucedida.

## Veja também

- [Documentação da biblioteca Csv no Elm](https://package.elm-lang.org/packages/elm-explorations/csv/latest/)
- [Exemplo completo de trabalho com CSV no Elm](https://github.com/elm-in-elm/complete-example/blob/master/src/Main.elm)