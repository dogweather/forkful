---
title:                "Concatenando strings"
html_title:           "Rust: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e por que você deve concatenar strings?

Concatenar strings é um recurso importante em programação, que permite que você junte diferentes cadeias de caracteres na mesma sequência. Isso é útil para criar strings dinâmicas e manipular dados de forma eficiente. Programadores usam concatenação de strings para criar mensagens personalizadas, gerar saída de dados formatados e muito mais.

## Como fazer:

O Rust oferece uma sintaxe simples e direta para concatenar strings. Use o operador `+` entre duas strings para uni-las em uma única string:

Exemplo 1:
Rust
```
let string1 = "Hello";
let string2 = "world!";
let combined_string = string1 + " " + string2;

println!("{}", combined_string);
```

Saída:
```
Hello world!
```

Você também pode usar a macro `format!` para concatenar várias strings em uma única linha:

Exemplo 2:
Rust
```
let name = "John";
let city = "London";
let age = 25;

let message = format!("My name is {}. I live in {} and I am {} years old.", name, city, age);

println!("{}", message);
```

Saída:
```
My name is John. I live in London and I am 25 years old.
```

## Detalhes aprofundados:

A concatenação de strings é uma técnica amplamente utilizada em linguagens de programação modernas e remonta aos primeiros dias da computação. Antes do surgimento de linguagens de programação orientadas a objetos, como C++, a concatenação de strings era muito mais complexa e exigia o uso de funções e ponteiros específicos para manipulação de strings.

Existem várias alternativas para a concatenação de strings em Rust, como o uso do método `push_str` para adicionar uma string a uma variável existente. No entanto, a concatenação usando o operador `+` é a forma mais comum e eficiente de obter resultados rápidos e legíveis.

No nível de implementação, o operador `+` utiliza a função `add` da trait `Add`, que é implementada para tipos de dados `String`, permitindo a concatenação entre eles.

## Veja também:

- Documentação oficial do Rust sobre concatenação de strings: https://doc.rust-lang.org/std/string/trait.Add.html
- Mais exemplos de concatenação de strings em Rust: https://riptutorial.com/pt_BR/rust/example/1374/concatenando-strings