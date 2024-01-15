---
title:                "Juntando cadeias de caracteres"
html_title:           "Gleam: Juntando cadeias de caracteres"
simple_title:         "Juntando cadeias de caracteres"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porque
Você já teve a necessidade de combinar diferentes strings em uma só? Talvez para criar uma URL dinâmica ou para exibir uma mensagem personalizada? O Gleam oferece uma maneira simples e eficiente de realizar essa tarefa através da concatenação de strings.

## Como Fazer
A função `<>` é utilizada para concatenar strings no Gleam. Veja um exemplo abaixo:
```
Gleam
let mensagem = "Olá" <> " mundo"
```
Neste caso, a variável `mensagem` seria igual a "Olá mundo". Podemos também concatenar mais de duas strings:
```
Gleam
let nome = "Maria"
let sobrenome = "Silva"
let nome_completo = nome <> " " <> sobrenome
```

Neste exemplo, a variável `nome_completo` seria igual a "Maria Silva". Além disso, podemos também adicionar valores de outras variáveis em nossa concatenação:
```
Gleam
let idade = 25
let mensagem = "Maria tem " <> idade <> " anos"
```
Aqui, a variável `mensagem` seria igual a "Maria tem 25 anos". Lembre-se que a função `<>` só funciona com strings, então é necessário converter outros tipos de dados para string antes de concatená-los.

## Deep Dive
Além da função `<>`, o Gleam também possui outras funções para manipular strings, como `slice`, `replace`, `trim`, entre outras. Além disso, é possível utilizar expressões regulares para realizar concatenações mais complexas. Para conhecer todas essas opções, confira a documentação oficial do Gleam sobre strings.

## Veja Também
- [Documentação oficial do Gleam sobre strings](https://gleam.run/documentation/std_lib/string/)
- [Tutorial sobre manipulação de strings no Gleam](https://dev.to/charlotterbee/guide-to-string-manipulation-in-gleam-2kli)
- [Exemplos práticos de concatenação de strings no Gleam](https://github.com/jordymoos/gleam-by-example/blob/master/strings.md)