---
title:                "Lendo um arquivo de texto"
html_title:           "Gleam: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Ler arquivos de texto é uma tarefa comum na programação, seja para extrair informações de um documento ou para processar dados de entrada. Aprender a ler arquivos de texto em Gleam pode expandir suas habilidades de programação e permitir que você trabalhe com diferentes tipos de dados.

## Como Fazer

A seguir, mostraremos como ler um arquivo de texto passo a passo com exemplos de código Gleam e a saída correspondente. Primeiro, você precisará definir um tipo para representar o conteúdo do arquivo:

```Gleam
type FileContent {
  lines: List(String)
}
```

Em seguida, importe o módulo `gleam/fs` para acessar as funções de leitura de arquivos:

```Gleam
import gleam/fs

pub fn read_file(path: String) -> FileContent {
  let result = fs.read_file(path)

  case result {
    Ok(contents) -> FileContent { lines: String.split("\n", contents) }
    Error(err) -> panic(err)
  }
}
```

Aqui, criamos uma função `read_file` que usa a função `fs.read_file` para ler o conteúdo de um arquivo e, em seguida, separar as linhas em uma lista de strings. Agora, podemos chamar essa função para ler um arquivo e ver a saída:

```Gleam
let file_content = read_file("exemplo.txt")

let primeira_linha = List.get(0, file_content.lines)
let segunda_linha = List.get(1, file_content.lines)

Assert.equal(file_content.lines, ["Este é um texto de exemplo.", "Ele contém algumas linhas para ler."])
Assert.equal(primeira_linha, "Este é um texto de exemplo.")
Assert.equal(segunda_linha, "Ele contém algumas linhas para ler.")
```

Ao extrair as linhas em uma lista, podemos acessá-las individualmente e compará-las com o que esperamos. Isso também nos permite trabalhar com o conteúdo do arquivo de maneira mais flexível e aplicar quaisquer transformações necessárias.

## Deep Dive

Embora tenhamos mostrado como ler um arquivo de texto simples neste artigo, é importante notar que existem muitas outras opções e métodos disponíveis no módulo `gleam/fs` para a leitura de arquivos de texto. Você pode aprender mais sobre eles lendo a documentação oficial do Gleam ou explorando por conta própria.

Além disso, tenha em mente que a leitura de arquivos de texto pode envolver outras tarefas, como tratamento de erros e conversão de tipos de dados. É importante ter uma compreensão sólida desses conceitos para ler e manipular arquivos de texto com eficiência.

## Veja Também

- [Documentação Oficial do Gleam](https://gleam.run/documentation)
- [Módulo `gleam/fs`](https://gleam.run/modules/gleam_fs.html)
- [Exemplos de leitura de arquivos em Gleam](https://github.com/search?q=language%3Agleam+fs.read_file&type=Code)