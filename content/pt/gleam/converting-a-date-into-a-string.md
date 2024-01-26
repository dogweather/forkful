---
title:                "Convertendo uma data em uma string"
date:                  2024-01-20T17:36:27.436017-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data para uma string é o processo de mudar uma representação de data, que é tipicamente armazenada num formato que a máquina entende, para um formato de texto legível pelos humanos. Programadores fazem isso para exibir datas de maneira compreensível em interfaces de usuário ou ao armazenar informações em arquivos de texto.

## Como Fazer:
```gleam
import gleam/io
import gleam/erlang.time.{Date, FormatError, from_iso_string, to_string}

pub fn main() {
  case from_iso_string("2023-04-12") {
    Ok(date) ->
      case to_string(date) {
        Ok(string) -> io.println(string)
        Error(_) -> io.println("Não foi possível converter a data para string.")
      }
    Error(_) ->
      io.println("Formato de data ISO inválido.")
  }
}
```
Saída esperada:
```
"12 de Abril de 2023"
```

## Aprofundando
Historicamente, a necessidade de converter datas em strings surgiu da limitação humana de não conseguir facilmente entender ou lembrar datas no formato de timestamp ou binário que os computadores utilizam internamente. Existem várias alternativas para realizar essa conversão em Gleam, como a biblioteca `gleam_datetime`, que oferece funções robustas para manipulação de datas. Em relação aos detalhes de implementação, as funções `from_iso_string` e `to_string` do módulo `gleam/erlang.time` servem como um bom ponto de partida, fazendo uso dos padrões de datas no formato ISO para conversão e proporcionando versatilidade no manuseio de datas em Gleam.

## Veja Também
- Documentação oficial do Gleam sobre tipos de tempo: https://hexdocs.pm/gleam_stdlib/gleam/erlang/time/
- Repositório GitHub da biblioteca `gleam_datetime`: https://github.com/gleam-experiments/datetime
- Especificação do formato de data ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
