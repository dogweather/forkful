---
title:                "Lendo um arquivo de texto"
date:                  2024-01-20T17:54:11.798839-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Ler um arquivo de texto é o processo de pegar dados de um arquivo em seu disco e trazê-los para dentro do seu programa. Programadores fazem isso para manipular, analisar ou transformar informações salvadas em um formato facilmente editável e armazenável.

## Como fazer:
Em Gleam, você pode ler um arquivo de texto usando a biblioteca `gleam/io`. Vamos a um exemplo:

```gleam
import gleam/io

fn read_file(file_path: String) -> Result(String, io.Error) {
  try file = io.open(file_path)
  try contents = io.read_to_string(file)
  io.close(file)
  Ok(contents)
}

pub fn main() {
  case read_file("caminho/para/seu/arquivo.txt") {
    Ok(contents) -> io.println(contents)
    Error(err) -> io.println("Oops! Algo deu errado: " ++ err)
  }
}
```
Saída de exemplo quando tudo dá certo:
```
Olá, Gleam!
```
Quando ocorre um erro:
```
Oops! Algo deu errado: Error(PermissionDenied)
```

## Mergulho Profundo
Historicamente, ler arquivos está entre as operações mais básicas em programação, porque permite que programas interajam com dados persistentes. Em Gleam, que é fortemente tipado e inspirado por Erlang/OTP, manipular arquivos de texto é seguro em termos de tipos e focado em performance. Alternativas para leitura de arquivos incluem streaming de linhas para trabalhar com arquivos grandes ou binários para dados que não são texto simples. Detalhes de implementação, como tratamento de erros e fechamento de arquivos, são cruciais para prevenir vazamentos de recursos e garantir a robustez da aplicação.

## Veja Também
- Documentação oficial de Gleam: [https://gleam.run](https://gleam.run)
- Erlang/OTP, a base de Gleam: [https://www.erlang.org/docs](https://www.erlang.org/docs)
