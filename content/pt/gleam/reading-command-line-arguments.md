---
title:                "Gleam: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Ler argumentos de linha de comando é uma habilidade fundamental para qualquer programador. Ao dominar essa técnica, você poderá criar programas mais interativos e flexíveis, capazes de receber entradas diretamente do usuário através da linha de comando.

## Como fazer

Para ler argumentos de linha de comando em Gleam, utilizamos a função `gleam/io.command_line.arguments`. Essa função retorna uma lista com os argumentos informados pelo usuário ao executar o programa. Veja o exemplo:

```
Gleam import gleam/io

fn main() {
    args := gleam/io.command_line.arguments()
    io.println("Olá, " ++ List.nth(args, 0))
}
```

Neste exemplo, a função `main` está recebendo o primeiro argumento informado pelo usuário e incorporando-o à mensagem de saudação exibida no terminal. Se o usuário executar o programa com o comando `gleam run hello.gleam Mundo`, a saída será `Olá, Mundo`.

## Profundando-se

Além de receber argumentos de texto simples, como no exemplo anterior, também é possível ler argumentos numéricos, booleanos e até mesmo arquivos de entrada. A função `gleam/io.command_line.parse_arguments` permite definir o tipo esperado para cada argumento, tornando seu programa ainda mais interativo e robusto.

## Veja também

- [Documentação oficial do Gleam sobre leitura de argumentos de linha de comando](http://gleam.run/documentation/#command-line-arguments)
- [Exemplo completo de programa em Gleam usando argumentos de linha de comando](https://github.com/gleam-lang/gleam/blob/master/examples/command_line_arguments.gleam)