---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:56:09.963952-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Ler argumentos da linha de comando é pegar pedaços de dados que os usuários passam para o programa ao executá-lo no terminal. Programadores fazem isso para personalizar a execução de um programa com base nas preferências do usuário ou necessidades específicas da tarefa em questão.

## Como Fazer:

Para ler argumentos de linha de comando em Gleam, podemos usar o módulo `gleam/io`. Vamos a um exemplo prático:

```gleam
import gleam/io

pub fn main(args: List(String)) {
  let arg_message = match args {
    [] -> 
      "Ops! Você esqueceu os argumentos."
    [first_arg | _rest] -> 
      first_arg
  }
  io.println(arg_message)
}
```
Se rodar o programa com `gleam run`, a saída será:
```
Ops! Você esqueceu os argumentos.
```
Agora, se passar argumentos na linha de comando, como `gleam run arg1`, a saída será:
```
arg1
```

## Aprofundamento

Historicamente, a leitura de argumentos da linha de comando remonta aos primórdios dos sistemas operacionais, quando a interface gráfica ainda nem existia. Existem várias bibliotecas e módulos em diferentes linguagens de programação que permitem essa funcionalidade. Em Gleam, a simplicidade do módulo `gleam/io` facilita essa operação.

Quanto à implementação, é comum que o primeiro elemento do array de argumentos seja o caminho do próprio programa. No Gleam, isso é abstraído, e `args` é só o que você passa para o programa, sem o caminho.

Outras linguagens como Rust ou Go têm pacotes dedicados com funcionalidades avançadas para manipular argumentos da linha de comando, como parsing de flags e subcomandos, mas em Gleam a ideia é manter o core simples.

## Veja Também

- Post no blog sobre parsing de argumentos de linha de comando em Rust: [https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)