---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:36:22.601378-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data de uma string é transformar texto em um formato de data reconhecível pelo sistema. Programadores fazem isso para manipular e armazenar datas em formatos úteis para análise e lógica de negócios.

## Como Fazer:
```gleam
import gleam/io
import gleam/should
import gleam/calendar.{Date, from_iso8601}

pub fn main() {
  let date_string = "2023-04-02"
  case from_iso8601(date_string) {
    Ok(date) -> io.println("Data convertida: " ++ date)
    Error(_error) -> io.println("Erro ao converter data.")
  }
}

// Saída esperada: "Data convertida: 2023-04-02"
```

## Aprofundando
Antes de termos padrões como ISO 8601, a interpretação de datas variava muito por cultura e localidade, resultando em confusão na programação global. Alternativas ao `from_iso8601` poderiam incluir parsers personalizados usando expressões regulares ou bibliotecas de terceiros que suportam múltiplos formatos. O detalhe de implementação crítico é garantir que a conversão considere diferentes fusos horários e estilos de datas, para assegurar a consistência e a exatidão dos dados de tempo.

## Veja Também
- Especificação ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
