---
title:                "Localizando e substituindo texto"
html_title:           "Rust: Localizando e substituindo texto"
simple_title:         "Localizando e substituindo texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e por que?

Substituir texto é uma tarefa comum para programadores. Basicamente, isso significa encontrar uma sequência de caracteres específica em um texto e substituí-la por outra. Os programadores fazem isso para corrigir erros, adicionar funcionalidades ou simplificar seu código.

## Como fazer:

Para substituir texto em Rust, podemos usar o método `replace()` da classe `String`.

```Rust
let texto = "Olá, mundo!";
let novo_texto = texto.replace("mundo", "tudo");

println!("{}", novo_texto);
```

Este código irá substituir a palavra "mundo" por "tudo" e imprimir "Olá, tudo!".

## Mergulho profundo:

A tarefa de substituir textos é muito antiga e já foi resolvida de várias maneiras. Uma abordagem comum é o algoritmo de busca e substituição Boyer-Moore, que foi desenvolvido em 1977.

Alternativamente, os programadores também podem usar expressões regulares para substituir padrões de texto específicos em uma string.

No Rust, o método `replace()` é implementado como parte do tipo `String`, que é uma estrutura de dados altamente otimizada para manipulação de texto.

## Veja também:

- Documentação oficial do Rust para o método `replace()`: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Site oficial do algoritmo Boyer-Moore: http://www-igm.univ-mlv.fr/~lecroq/string/node14.html
- Site para testar expressões regulares: https://regexr.com/