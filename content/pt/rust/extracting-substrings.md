---
title:                "Extraindo subcadeias"
html_title:           "Rust: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma tarefa comum em programação, seja para manipular dados de texto ou para obter informações específicas de uma determinada string. Em Rust, existem várias maneiras de realizar essa tarefa com eficiência e elegância. Neste artigo, vamos explorar como extrair substrings em Rust e como essa funcionalidade pode facilitar o seu trabalho como programador.

## Como fazer

Extrair substrings em Rust é bastante simples e podemos utilizar algumas funções e métodos nativos para realizar essa tarefa. Por exemplo, se tivermos uma string chamada "Hello World" e quisermos extrair apenas a palavra "World", podemos fazer da seguinte forma:

```Rust
let string = "Hello World"; // Declara uma string
let substring = &string[6..]; // Extrai a substring a partir do índice 6 até o final da string
println!("{}", substring); // Imprime a substring "World"
```

Outra forma de extrair substrings é utilizando o método `split()` que quebrará a string em partes, de acordo com um determinado caractere ou padrão. Por exemplo, podemos utilizar o método `split()` para extrair apenas os dígitos de uma string, como mostrado no exemplo abaixo:

```Rust
let string = "1234abc"; // Declara uma string
let digits: String = string.split(char::is_alphabetic).collect(); // Extrai apenas os dígitos da string e armazena em uma variável
println!("{}", digits); // Imprime a string com apenas os dígitos "1234"
```

Além disso, Rust também oferece a função `matches()` que permite extrair múltiplas substrings de uma string que correspondam a um determinado padrão. Veja um exemplo:

```Rust
let string = "Brasil 2021"; // Declara uma string
let substrings: Vec<&str> = string.matches(char::is_numeric).collect(); // Extrai todas as substrings numéricas e armazena em um vetor
println!("{:?}", substrings); // Imprime as substrings ["2", "0", "2", "1"]
```

## Profundidade

Ao trabalhar com extrair substrings em Rust, é importante entender alguns conceitos fundamentais, como os índices de strings e o uso de referências imutáveis. É importante lembrar que em Rust, as strings são guardadas em formato UTF-8 e, por isso, seus índices não correspondem necessariamente a cada caractere individual.

Além disso, ao utilizar referências imutáveis ao extrair substrings, estamos apenas fazendo uma "visualização" da string original, sem modificar a sua posição na memória. Isso é importante para garantir um código seguro e sem erros.

## Veja também

- Documentação oficial de strings em Rust: https://doc.rust-lang.org/std/string/index.html
- Artigo sobre manipulação de strings em Rust: https://blog.logrocket.com/string-manipulation-in-rust/
- Vídeo tutorial de como extrair substrings em Rust: https://www.youtube.com/watch?v=I_icqEnBMz8