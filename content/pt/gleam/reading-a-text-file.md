---
title:                "Gleam: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler um arquivo de texto em Gleam?

Se você está iniciando sua jornada na programação ou já é um programador experiente, a leitura de arquivos de texto é uma tarefa fundamental para várias aplicações. Com Gleam, uma linguagem de programação funcional moderna, é possível ler e manipular arquivos de texto de forma simples e eficiente. Neste artigo, vamos explorar como realizar essa tarefa em Gleam e aprofundar nossos conhecimentos sobre o assunto.

## Como fazer

A primeira etapa para ler um arquivo de texto em Gleam é importar o módulo `gleam` e seus submódulos `io` e `file`. Em seguida, utilizamos a função `File.read()` para ler o conteúdo de um arquivo de texto. Vamos dar uma olhada em um exemplo de código para entender melhor:

```Gleam
import gleam/io
import gleam/file

let file = "/caminho/para/o/arquivo.txt"
let conteudo = File.read(file)

io.print("O conteúdo do arquivo é:")
io.print(conteudo)
```

Neste código, importamos os módulos necessários e em seguida, utilizamos a função `File.read()` para ler o conteúdo do arquivo de texto. Por fim, imprimimos o conteúdo do arquivo utilizando a função `print()` do módulo `io`. Ao executar esse código, o resultado seria algo como:

```
O conteúdo do arquivo é:
Este é um arquivo de texto!
```

## Mergulho Profundo

Além de simplesmente ler o conteúdo de um arquivo, podemos realizar diversas manipulações utilizando recursos de Gleam. Por exemplo, podemos utilizar a função `String.split()` para dividir o conteúdo do arquivo em linhas ou palavras, ou ainda utilizar a função `String.slice()` para extrair partes específicas do conteúdo. Também é possível utilizar o módulo `io` para escrever em um arquivo de texto utilizando a função `io.write()`.

Outro recurso interessante é o `pattern matching`, que pode ser utilizado para processar o conteúdo do arquivo de forma mais precisa. Podemos, por exemplo, utilizar `match` para verificar se determinadas palavras ou padrões estão presentes no texto e tomar ações diferentes de acordo com cada caso.

A leitura de arquivos de texto em Gleam é uma tarefa versátil e essencial para diversas aplicações. Com os recursos e funcionalidades da linguagem, é possível realizar manipulações sofisticadas e automatizar processos de forma eficiente.

# Veja também

1. [Documentação oficial de Gleam](https://gleam.run/)
2. [Guia de introdução à programação funcional com Gleam](https://dev.to/gleam_lang/an-introduction-to-functional-programming-with-gleam-2h57)
3. [Exemplos práticos de Gleam no GitHub](https://github.com/search?q=language%3Agleam)