---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:56:16.713184-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar a existência de um diretório é essencial para evitar erros ao tentar ler ou escrever arquivos nele. Programadores fazem isso para garantir a integridade de operações de arquivo e para reagir adequadamente caso o diretório não exista.

## Como Fazer:
```gleam
import gleam/io
import gleam/erlang/error

pub fn check_directory_exists(dir: String) -> Bool {
  try io.file_info(dir)
  |> result.map(fn(_) { true })
  |> result.catch(fn(error) {
    case error {
      | error.NotFound -> false
      | _ -> error.return()
    }
  })
}

// Exemplo de uso
fn main() {
  let dir_exists = check_directory_exists("path/to/dir")
  io.println(dir_exists) // Saída: true ou false
}
```

## Aprofundamento
No passado, verificar a existência de um diretório envolvia interações diretas com o sistema operacional usando comandos shell ou funções da biblioteca padrão de baixo nível. Alternativas modernas, como na linguagem Gleam, utilizam funções encapsuladas que abstraem detalhes específicos do sistema operacional.

A função `io.file_info` do Gleam verifica metadados de um arquivo ou diretório. Um erro `NotFound` indica que o diretório não existe. Outros erros são propagados, permitindo que o programa os trate conforme necessário. 

Gleam, ao ser executado na Erlang VM, se beneficia das poderosas bibliotecas e funções de manipulação de arquivos do Erlang. No entanto, ao contrário do Erlang, Gleam é fortemente tipado e possui uma sintaxe mais amigável e moderna, proporcionando uma abordagem robusta e menos propensa a erros para operações de sistema de arquivos.

## Veja Também
- [Gleam Language GitHub](https://github.com/gleam-lang/gleam)
- [Erlang File Operations](https://erlang.org/doc/man/file.html)