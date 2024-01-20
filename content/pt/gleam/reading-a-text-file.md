---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Ler um arquivo de texto é o processo de extrair dados de um arquivo e colocá-los na memória do computador para processamento posterior. Os programadores fazem isso para manipular e analisar dados existentes, que geralmente são armazenados em arquivos de texto.

## Como fazer:

Aqui vai um exemplo simples de como ler um arquivo de texto em Gleam.

```gleam
import gleam/filesystem
import gleam/bit_builder.{append, from_string}
import gleam/result.{map, map_error}

pub fn ler_arquivo(caminho: String) {
  let caminho_para_arquivo = filesystem.file(caminho)

  filesystem.read(caminho_para_arquivo)
  |> map(result.from_string)
  |> map_error(result.from_string)
}
```

Nesse exemplo, a função `ler_arquivo` lê o arquivo de texto localizado no caminho especificado.

## Mergulho Profundo

Historicamente, a leitura de arquivos de texto tem sido uma parte crucial de muitas operações de computação, remontando aos primeiros dias de programação. Ela oferece uma maneira simples, porém eficaz, de armazenar e recuperar dados.

Existem alternativas para a leitura de arquivos de texto, como a leitura de arquivos binários, arquivos XML, arquivos JSON, etc. A escolha depende amplamente dos requisitos de seu projeto.

Em termos de implementação, Gleam usa funções de Elixir subjacentes para interagir com o sistema de arquivos. Assim, a leitura de arquivos é feita de forma eficiente e segura.

## Veja Também

Aqui estão alguns links úteis para aprender mais sobre programação Gleam e leitura de arquivos de texto: