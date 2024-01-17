---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Rust: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Deletar caracteres que correspondem a um padrão significa remover de uma string todos os caracteres que seguem um determinado formato ou regra. Os programadores geralmente fazem isso para limpar os dados de entrada, tornando-os mais fáceis de serem lidos e manipulados pelo programa.

## Como Fazer:
Existem várias maneiras de deletar caracteres correspondentes a um padrão em Rust. Uma opção é usar o método `replace` da struct `String`, que substitui um determinado padrão por uma string vazia. Por exemplo, para remover todos os dígitos de uma string, podemos usar o código:

```
let string = String::from("Texto123");
let resultado = string.replace(char::is_numeric, "");
// resultado é "Texto"
```

Outra abordagem é usar a função `regex_replace` do módulo `regex`, que permite usar expressões regulares para encontrar e substituir padrões em uma string. O seguinte código remove todos os caracteres não alfanuméricos de uma string:

```
use regex::Regex;

let string = String::from("Texto@#$");
let re = Regex::new(r"[^A-Za-z0-9]").unwrap();
let resultado = re.replace_all(&string, "");
// resultado é "Texto"
```

## Mergulho Profundo:
A utilização de expressões regulares para manipulação de strings remonta aos primórdios da computação. Antes da criação de linguagens de programação específicas para manipulação de strings, os programadores geralmente precisavam usar comandos de linha de comando ou ferramentas externas para realizar operações de busca e substituição em arquivos de texto.

Atualmente, existem várias bibliotecas de expressões regulares disponíveis para Rust, como o módulo `regex`, que oferece uma ampla gama de funcionalidades para trabalhar com padrões de strings.

Além disso, existem outras maneiras de manipular strings em Rust, como o módulo `str`, que oferece métodos como `trim`, `split` e `to_lowercase` para simplificar a manipulação de strings.

## Veja Também:
- [Documentação do Rust sobre o método `replace`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Documentação do módulo `regex` para Rust](https://docs.rs/regex/1.3.1/regex/)
- [Guia do desenvolvedor para string em Rust](https://www.ncameron.org/blog/guide-developing-rust-cli-applications-part-2-strings/)