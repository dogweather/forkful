---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Ler argumentos da linha de comando é uma maneira de aceitar inputs do usuário quando ele executa seu programa. Fazemos isso para que nossos programas possam ser personalizados cada vez que são executados.

## Como Fazer:

No Gleam, você pode ler argumentos da linha de comando da seguinte maneira:

```Gleam
import gleam/io.{println, command_args}

fn main(args: List(String)) {
    let arg_list = command_args()
    println(arg_list)
}
```

Por exemplo, se o programa é executado com `./program arg1 arg2 arg3`, o output será:

```Gleam
["arg1", "arg2", "arg3"]
```

## Imersão Profunda:

A prática de ler argumentos da linha de comando vem de quando a interação com computadores era feita principalmente por meio de terminais de texto. Ainda é uma boa prática, particularmente para scripting e automação.

Como alternativa, você poderia usar entradas padrão ou ler de um arquivo, mas na maioria dos casos, o uso de argumentos de linha de comando é mais direto.

A função `command_args` do Gleam precisa ser definida no arquivo principal do seu programa (conhecido como `main module`). Ela retorna uma lista de strings que são os argumentos passados ​​para o seu programa.

## Veja Também:

[Documentação Oficial do Gleam sobre Entrada/Saída(IO)](https://gleam.run/book/tour/io.html)

[Documentação do Rust sobre Argumentos da Linha de Comando](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)