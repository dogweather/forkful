---
title:                "Rust: Convertendo uma string para minúsculas"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Converter uma string para letras minúsculas é uma tarefa comum em programação, especialmente em linguagens de programação sensíveis a maiúsculas e minúsculas como Rust. Ao converter uma string para letras minúsculas, podemos garantir consistência e facilitar a comparação de strings para fins de classificação, busca e outros usos.

## Como fazer a conversão em Rust

Para converter uma string para letras minúsculas em Rust, podemos usar o método `to_lowercase()` da struct `String`. Veja um exemplo de código abaixo:

```Rust
let string = String::from("EXEMPLO DE STRING");
let lowercase_string = string.to_lowercase();

println!("String original: {}", string);
println!("String em letras minúsculas: {}", lowercase_string);
```

Output:

```
String original: EXEMPLO DE STRING
String em letras minúsculas: exemplo de string
```

Podemos ver que a string original é convertida com sucesso para letras minúsculas.

## Mergulho profundo

Ao chamar o método `to_lowercase()` em uma string em Rust, o valor retornado é uma nova `String`, com letras minúsculas. Isso significa que a string original não é modificada. Além disso, devemos ter em mente que as regras de conversão de maiúsculas e minúsculas podem variar dependendo do idioma. Por exemplo, a letra "İ" em turco não é convertida para "i", mas para "ı". Portanto, é importante estar ciente dessas diferenças ao trabalhar com strings multilíngues.

## Veja também

- Documentação oficial de Rust sobre o método `to_lowercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Artigo sobre strings e suas manipulações em Rust: https://dev.to/rosolv/working-with-strings-in-rust-115o
- Tutorial em vídeo sobre como converter strings para letras minúsculas em Rust: https://www.youtube.com/watch?v=r7Lt65-_PnM