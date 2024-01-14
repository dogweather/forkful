---
title:                "Haskell: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings em Haskell?

Extrair substrings é uma tarefa bastante comum em programação, especialmente quando lidamos com manipulação de texto ou análise de dados. Em Haskell, existem diversas ferramentas e funções para nos auxiliar nessa tarefa, tornando o processo mais fácil e eficiente. Neste artigo, vamos explorar o porquê de se extrair substrings e como fazer isso em Haskell.

## Como fazer

Para extrair substrings em Haskell, podemos utilizar a função `take` e `drop`. A função `take` recebe como argumentos a quantidade de elementos que queremos extrair e a lista de origem. Já a função `drop` recebe como argumentos a quantidade de elementos que queremos pular e a lista de origem. Em seguida, utilizamos a função `length` para determinar o tamanho da substring que queremos extrair.

Um exemplo prático seria o seguinte:

```
-- código Haskell
str = "Olá, mundo!"
take (length "Olá") str
drop (length "Olá, ") str
```

O output seria o seguinte:

```
-- output
"Olá"
"mundo!"
```

Podemos também utilizar a função `substring` do pacote `Data.Text` para extrair substrings. Ela recebe como argumentos o índice inicial e o índice final da substring que queremos extrair. Veja um exemplo:

```
-- código Haskell
import Data.Text (substring)

str = "Hello World"
substring 0 4 str
substring 6 10 str
```

O output seria o seguinte:
```
-- output
"Hello"
"World"
```

## Mergulho profundo

Extrair substrings pode ser uma tarefa simples, mas é importante entender alguns detalhes por trás disso. Em Haskell, as strings são tratadas como listas de caracteres, portanto, podemos utilizar as mesmas funções e conceitos utilizados para manipulação de listas. Além disso, ao utilizar a função `take` e `drop`, é importante lembrar que elas retornam novas listas, portanto, é necessário atribuí-las a uma variável para podermos utilizá-las posteriormente.

Outro ponto importante é o uso do pacote `Data.Text`, que é mais eficiente para manipulação de textos em comparação com a função `take` e `drop`, que são mais adequadas para manipular listas em geral. Portanto, é importante entender qual ferramenta é mais adequada para cada situação.

## Veja também

- [Documentação da função `take` e `drop`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#g:14)
- [Documentação da função `substring`](https://hackage.haskell.org/package/text-1.2.3.2/docs/Data-Text.html#v:substring)