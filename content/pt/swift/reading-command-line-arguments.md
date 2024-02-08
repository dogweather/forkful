---
title:                "Lendo argumentos da linha de comando"
aliases:
- pt/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:56:55.154514-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Ler argumentos da linha de comando permite que programas Swift recebam inputs externos ao serem executados. Programadores fazem isso para personalizar a execução de apps via terminal ou scripts, baseando-se em dados fornecidos por usuários ou outros programas.

## How to:
Ler argumentos em Swift é simples. A propriedade `CommandLine.arguments` retorna um array de strings com os argumentos passados. Veja o exemplo:

```Swift
// main.swift
for argument in CommandLine.arguments {
    print(argument)
}
```

Executando `swift main.swift olá mundo`, vai resultar em:

```
main.swift
olá
mundo
```

Note que o primeiro argumento é sempre o caminho do script.

## Deep Dive
Historicamente, ler argumentos da linha de comando é um conceito herdado do Unix e de sistemas C-like, como C e C++. Em Swift, a classe `CommandLine` contém recursos para esta finalidade.

Alternativas para leitura de comandos incluem o uso de bibliotecas de terceiros, como `SwiftCLI` ou `Commander`, que oferecem mais funcionalidades e uma interface mais refinada para lida com inputs complexos.

Quando se implementa a leitura de argumentos, considere validar e parsear cada argumento para garantir que eles sejam adequados para seu uso no programa. Isso pode envolver a conversão de tipos de dados, a verificação de presença de flags e a manipulação de erros.

## See Also
- Documentação Oficial da Swift: [https://swift.org/documentation/](https://swift.org/documentation/)
- Artigo "Command-Line Argument Parsing using Swift Package Manager": [https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial)
- SwiftCLI no GitHub: [https://github.com/jakeheis/SwiftCLI](https://github.com/jakeheis/SwiftCLI)
- Commander no GitHub: [https://github.com/kylef/Commander](https://github.com/kylef/Commander)
