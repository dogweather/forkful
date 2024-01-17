---
title:                "Leitura de argumentos da linha de comando"
html_title:           "Gleam: Leitura de argumentos da linha de comando"
simple_title:         "Leitura de argumentos da linha de comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

"## O que & Por quê?"
Ler argumentos da linha de comando é um processo comum em programação, em que um programa recebe informações fornecidas pelo usuário diretamente na linha de comando, ao invés de utilizar uma interface gráfica. Isso permite aos programadores criar aplicativos mais versáteis e interativos.

"## Como fazer:"
Para ler argumentos da linha de comando em Gleam, utilizamos a função ```Gleam.Args.parse```. Veja o exemplo abaixo para um programa que recebe dois argumentos, o nome e a idade do usuário:
```
fn main() {
    let args = Gleam.Args.parse();

    Gleam.IO.print("Olá, " <> args[0] <> "! Você tem " <> args[1] <> " anos.");
}
```

Exemplo de utilização:
```
$ gleam run programa.gleam João 25
Olá, João! Você tem 25 anos.
```

"## Mergulho Profundo:"
Ler argumentos da linha de comando é uma técnica que existe há muito tempo, sendo utilizada em muitas linguagens de programação. Embora ainda seja uma funcionalidade essencial em muitos programas, existem alternativas como a utilização de arquivos de configuração. Em Gleam, a função ```Gleam.Args.parse``` utiliza a biblioteca de parsers opt_util para realizar a análise dos argumentos.

"## Veja também:"
- Documentação da função ```Gleam.Args.parse```: https://gleam.run/lib/op/arg_parse.html
- Tutorial de Gleam no site oficial: https://gleam.run/getting-started
- Artigo sobre a história da leitura de argumentos da linha de comando: https://en.wikipedia.org/wiki/Command-line_interface#Arguments