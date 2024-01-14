---
title:    "Elm: Procurando e substituindo texto"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que usar a busca e substituição de texto em Elm?

A busca e substituição de texto é uma tarefa comum em programação, especialmente em fornecer uma interface amigável para os usuários interagirem com seus aplicativos. Em Elm, é importante entender como realizar essa tarefa de forma eficiente e sem erros.

## Como fazer

Para realizar a busca e substituição de texto em Elm, usamos a função `String.replace` que leva três argumentos: a string a ser substituída, a string de busca e a string de substituição. Aqui está um exemplo de como usá-lo:

```Elm
import String exposing (replace)
 
texto = "Bem-vindo ao meu blog!"
 
novoTexto = replace "bem-vindo" "Oi" texto
 
-- resultado: "Oi ao meu blog!"
```

Podemos ver que a string "Bem-vindo" foi substituída por "Oi" no texto original. Além disso, também podemos usar a função `String.contains` para verificar se uma string contém outra string antes de usá-la na substituição.

## Mergulho Profundo

Além da função `String.replace`, Elm também tem a função `String.replaceRange` que permite substituir parte de uma string com outra string. Isso pode ser útil em casos onde não queremos substituir a string inteira, mas apenas uma parte dela.

Outra função útil é `String.split` que divide uma string em uma lista de strings, separando-as em um determinado caracter. Isso pode ser útil em casos onde queremos trabalhar com partes específicas de uma string e, em seguida, substituí-las separadamente.

## Veja Também

- Documentação oficial do Elm sobre busca e substituição de texto: https://package.elm-lang.org/packages/elm/string/latest/String
- Mais exemplos de busca e substituição de texto em Elm: https://elmprogramming.com/string-manipulation-elm.html