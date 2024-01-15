---
title:                "Encontrando o comprimento de uma string."
html_title:           "Rust: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente, sempre precisará encontrar a extensão de uma string em seus projetos. Saber a extensão de uma string é importante para várias tarefas, como formatação, validação e manipulação de dados. Neste artigo, vamos explorar como encontrar a extensão de uma string em Rust de forma eficiente.

## Como fazer

Para encontrar a extensão de uma string em Rust, usamos o método `len ()`. Este método retorna um número inteiro que representa o comprimento da string passada como argumento. Aqui está um exemplo de código que encontra a extensão de uma string e imprime o resultado:

```
Rust
let minha_string = "Olá, mundo!";
let extensão = minha_string.len ();
println! ("A extensão da string é: {}", extensão);
```
A saída deste exemplo será: "A extensão da string é: 13". Isso significa que a string "Olá, mundo!" tem 13 caracteres.

É importante notar que o método `len ()` conta o número de bytes em uma string, e não o número de caracteres ou grafemas. Isso pode ser um pouco confuso, já que muitos caracteres em Unicode podem ser representados por mais de um byte.

Se você precisar encontrar a extensão de uma string específica em Unicode, há duas opções: usar o método `graphemes ()`, que retorna um iterador de todos os grafemas em uma string, ou usar o crate `unicode_segmentation`, que oferece uma série de métodos úteis para trabalhar com Unicode em Rust.

## Mergulho profundo

Agora que sabemos como encontrar a extensão de uma string em Rust, vamos entender como o método `len ()` funciona por trás dos panos.

Em Rust, as strings são representadas como fatias (`slices`) que apontam para uma parte da memória que contém os bytes correspondentes. Ao acessar o método `len ()`, o compilador Rust verifica o tamanho do array de bytes que a string está apontando e retorna esse valor.

Isso significa que o método `len ()` é muito eficiente, pois não realiza nenhuma iteração ou cálculo complexo - ele simplesmente retorna o tamanho do array de bytes.

## Veja também

Aqui estão alguns links úteis para você explorar mais sobre strings em Rust:

- Documentação oficial do Rust sobre strings: https://doc.rust-lang.org/std/string/
- Crate `unicode_segmentation`: https://crates.io/crates/unicode_segmentation
- Tutorial de strings em Rust: https://www.tutorialspoint.com/rust/rust_strings.htm