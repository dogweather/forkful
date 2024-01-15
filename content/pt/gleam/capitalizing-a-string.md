---
title:                "Capitalizando uma string"
html_title:           "Gleam: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Sabemos que ao trabalhar com cadeias de caracteres (strings) em programação, é comum nos depararmos com a necessidade de capitalizar essas strings. Isso pode ser útil para fins estéticos, formatação de dados ou até mesmo para melhorar a legibilidade do código.

## Como fazer

Em Gleam, existe uma função simples para capitalizar uma string, chamada `String.capitalize/1`. Veja como podemos usá-la em um exemplo:

```Gleam
import gleam/string

let name = "jose"
let capitalized_name = String.capitalize(name)

//com a função de capitalização, o valor de `capitalized_name` será "Jose"
```

Neste exemplo, a função `String.capitalize/1` recebe a string `jose` como entrada e retorna uma nova string com a primeira letra em maiúsculo. Simples, não é?

Podemos também combinar essa função com outras para capitalizar apenas a primeira letra de cada palavra em uma string, como no exemplo abaixo:

```Gleam
import gleam/string

let full_name = "jose rodrigues"
let capitalized_name = String.split(full_name, " ")
  |> List.map(\name -> String.capitalize(name))
  |> String.join(" ")

//com a função de capitalização, o valor de `capitalized_name` será "Jose Rodrigues"
```

Neste caso, usamos a função `String.split/2` para separar a string em duas partes (o primeiro e o último nome), e então usamos `List.map` para aplicar a função de capitalização em cada parte. Depois, usamos `String.join/2` para juntar as duas partes novamente em uma única string.

## Mergulho Profundo

A função `String.capitalize/1` é bem simples, mas existem outras maneiras de capitalizar strings em Gleam. Por exemplo, podemos criar nossa própria função utilizando o recurso de pattern-matching, que nos permite definir diferentes saídas para diferentes entradas. Veja um exemplo:

```Gleam
fn capitalize_name(name) {
  //verifica se a string está vazia
  case String.length(name) {
    //caso esteja vazia, retorna uma string vazia
    0 -> ""
    //caso tenha apenas uma letra, retorna a mesma letra
    1 -> name
    //caso tenha mais de uma letra, retorna a primeira letra em maiúsculo e o restante em minúsculo
    _ -> 
      let first_letter = String.slice(name, 0, 1)
      let rest = String.slice(name, 1, String.length(name))
      first_letter |> String.capitalize() <> String.to_lower(rest)
  }
}

let name = "jose"

//com a nossa função de capitalização, o valor de `capitalized_name` será "Jose"
let capitalized_name = capitalize_name(name)
```

Neste exemplo, usamos a função `String.length/1` para verificar o tamanho da string e, a partir disso, definimos diferentes saídas usando o `case`. Isso nos permite ter mais controle sobre a forma como a string será capitalizada.

Além disso, existem também outras funções em Gleam que podem ser usadas em conjunto com `String.capitalize/1`, como `String.to_upper/1` e `String.to_title/1`, que capitalizam todas as letras da string ou apenas as primeiras letras de cada palavra, respectivamente.

## Veja também

- Documentação oficial de Gleam sobre a função `String.capitalize/1`: https://gleam.run/modules/gleam/string.html#fn.capitalize
- Outras opções de string manipulation em Gleam: https://gleam.run/modules/gleam/string.html