---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever um arquivo de texto é salvar informações em um arquivo no seu disco. Programadores fazem isso para registrar dados, configurar sistemas, ou salvar resultados para uso posterior.

## Como Fazer:
```gleam
import gleam/io
import gleam/erlang

pub fn write_to_file() {
  let data: String = "Olá, Gleam!"
  try result = io.write_file("saudacao.txt", data)
  case result {
    Ok(_) -> io.print("Arquivo escrito com sucesso!")
    Error(error) -> io.print("Erro ao escrever o arquivo: " <> error)
  }
}
```
Saída após execução:
```
Arquivo escrito com sucesso!
```

## Mergulho Profundo:
Historicamente, a escrita de arquivos é essencial para armazenamento de dados persistentes. Em Gleam, que compila para Erlang, a escrita de arquivos é manipulada por módulos que abstraem as funções do sistema operacional. Alternativas de implementação podem incluir uso de bancos de dados ou serviços de armazenamento na nuvem, mas a escrita local em arquivos permanece uma operação fundamental na programação.

## Veja Também:
- Documentação oficial de Gleam: [https://gleam.run](https://gleam.run)
- Tutorial sobre manipulação de arquivos em Erlang (em inglês): [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Guia introdutório de Sistemas de Arquivos (em inglês): [https://en.wikipedia.org/wiki/File_system](https://en.wikipedia.org/wiki/File_system)
