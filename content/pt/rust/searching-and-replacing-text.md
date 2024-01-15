---
title:                "Busca e substituição de texto"
html_title:           "Rust: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Substituir texto pode ser uma tarefa tediosa e demorada quando feita manualmente. Com a ajuda de uma linguagem de programação como Rust, podemos automatizar esse processo e economizar tempo e esforço.

## Como fazer

Para realizar a substituição de texto em Rust, podemos usar o método `replace` da estrutura `String`. Vejamos um exemplo abaixo:

```Rust
let mut my_string = String::from("Sou um texto que precisa ser substituído");
let new_string = my_string.replace("Sou", "Era");
```

Neste exemplo, usamos o método `replace` para substituir a palavra "Sou" por "Era" na nossa string `my_string`. Agora, a variável `new_string` contém o texto "Era um texto que precisa ser substituído". Podemos também usar o método `replace` em um texto que esteja armazenado em um arquivo, como mostrado abaixo:

```Rust
use std::fs;

let my_file = fs::read_to_string("exemplo.txt").expect("Não foi possível ler o arquivo.");
let new_file = my_file.replace("texto", "arquivo");
```

Neste exemplo, o conteúdo do arquivo "exemplo.txt" é lido e armazenado na variável `my_file`. Em seguida, usamos o método `replace` para substituir a palavra "texto" por "arquivo" nesse conteúdo. O resultado é armazenado na variável `new_file`.

## Mergulho profundo

Além do método `replace`, Rust também nos oferece outras opções de substituição de texto. Por exemplo, o método `replacen` pode ser usado para substituir apenas uma determinada quantidade de ocorrências de uma palavra em uma string. Podemos especificar essa quantidade como um argumento na função, como mostrado abaixo:

```Rust
let mut my_string = String::from("Este é um exemplo de frase que contém a palavra exemplo três vezes.");
let new_string = my_string.replacen("exemplo", "exemplar", 2);
```

Neste exemplo, apenas as duas primeiras ocorrências da palavra "exemplo" serão substituídas por "exemplar", resultando no texto "Este é um exemplar de frase que contém a palavra exemplo uma vez.". 

## Veja também

- [Documentação oficial de Rust](https://www.rust-lang.org/pt-BR/)
- [Curso gratuito de Rust para iniciantes](https://www.udemy.com/course/aprenda-a-linguagem-rust/)
- [Tutorial de substituição de texto em Rust](https://www.tutorialspoint.com/replace-text-in-a-string-using-rust)