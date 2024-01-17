---
title:                "Extraindo substrings"
html_title:           "Gleam: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Extrair substrings é o ato de selecionar parte de uma string maior. Programadores frequentemente fazem isso para manipular e trabalhar com pedaços específicos de texto, em vez de lidar com a string inteira.

## Como fazer:

As duas principais funções usadas para extrair substrings em Gleam são `String.slice` e `String.substring`.

```
Gleam import String

pub fn main() {
  let string = "Olá, mundo!"

  //Usando a função "slice"
  String.slice(string, 0, 4) // Saída: "Olá,"

  //Usando a função "substring"
  String.substring(string, 0, 4) // Saída: "Olá," 
}
```

## Mergulho Profundo:

Extrair substrings tem sido uma técnica comum na programação e é amplamente utilizada em diferentes linguagens de programação. Embora Gleam tenha apenas duas funções dedicadas para isso, outras linguagens, como JavaScript, possuem várias opções de função, dependendo do que o programador deseja alcançar. Além disso, ao trabalhar com Unicode, é importante usar as funções `String.slice_utf8` e `String.substring_utf8` para garantir resultados precisos.

## Veja também:

- [Documentação oficial do Gleam](https://gleam.run/)
- [Tutorial sobre strings em Gleam](https://gleam.run/book/tour/strings.html)
- [Artigo sobre manipulação de strings em Gleam](https://blog.revillweb.com/2021/07/16/how-to-work-with-strings-in-gleam/)