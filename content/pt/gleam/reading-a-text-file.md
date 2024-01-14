---
title:                "Gleam: Lendo um arquivo de texto"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma habilidade fundamental para programadores e é necessário em uma variedade de situações, desde analisar dados até implementar recursos em um programa. Neste artigo, vamos explorar como podemos ler arquivos de texto usando a linguagem de programação Gleam.

## Como fazer

Para ler um arquivo de texto em Gleam, primeiro precisamos abrir o arquivo usando a função `File.open/1` e passar o caminho do arquivo como argumento. Em seguida, podemos usar a função `File.read/1` para ler o conteúdo do arquivo em uma variável. Aqui está um exemplo de código que lê um arquivo de texto e imprime o seu conteúdo na tela:

```Gleam
import File

// Abrir o arquivo
let file = File.open("meu_arquivo.txt")

// Ler o conteúdo do arquivo
let conteudo = File.read(file)

// Imprimir o conteúdo na tela
IO.print(conteudo)
```

O código acima assume que existe um arquivo de texto chamado `meu_arquivo.txt` no mesmo diretório que o arquivo Gleam está sendo executado. É importante lembrar que a função `File.open/1` retorna um _handle_ para o arquivo, que deve ser fechado após a leitura utilizando a função `File.close/1`.

## Mergulho Profundo

A função `File.read/1` retorna o conteúdo do arquivo como uma _string_, mas se quisermos analisar o conteúdo linha por linha, podemos usar a função `IO.stream/1` juntamente com a função `File.lines/1`. Isso nos permite ler o arquivo linha por linha sem carregá-lo inteiro na memória. Aqui está um exemplo:

```Gleam
import File

// Abrir o arquivo
let file = File.open("meu_arquivo.txt")

// Criar um stream com as linhas do arquivo
let linhas_stream = IO.stream(file) |> File.lines

// Imprimir cada linha
for linha in linhas_stream do
  IO.print(linha)
end
```

Além disso, a função `File.read/1` também pode ser utilizada para ler arquivos de texto em outros formatos, como CSV ou JSON, e o conteúdo do arquivo pode ser manipulado da forma que for necessário.

## Veja também

- [Documentação Gleam para leitura de arquivos](https://gleam.run/modules/io/file.html)
- [Gleam Playground - exemplo de leitura de arquivo](https://gleam.run/playground/?code=import%20File%0A%20%20%0A%2F%2F%20Abrir%20o%20arquivo%0Alet%20file%20%3D%20File.open(%22meu_arquivo.txt%22)%0A%20%20%0A%2F%2F%20Ler%20o%20conte%C3%BAdo%20do%20arquivo%0Alet%20conteudo%20%3D%20File.read(file)%0A%20%20%0A%2F%2F%20Imprimir%20o%20conte%C3%BAdo%20na%20tela%0AIO.print(conteudo))