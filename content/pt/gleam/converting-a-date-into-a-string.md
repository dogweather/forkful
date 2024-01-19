---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma data em uma string no Gleam: um guia prático

## O Que & Por Quê?

A conversão de uma data para uma string transforma um objeto de data em um formato textual que pode ser facilmente exibido, compartilhado ou armazenado. Programadores fazem isso porque é mais fácil trabalhar com datas em formatos de string.

## Como Fazer:

Primeiro, a biblioteca "gleam/time" deve ser importada. Vamos converter a data atual em uma string:

```Gleam
import gleam/time.{now, format}

fn converter_data_string() {
  let data_atual = now()
  let formato_string = "YYYY-MM-DD"
  
  format(data_atual, formato_string)
  |> io.println
}
```

Ao executar este código, você verá a data atual exibida no formato "YYYY-MM-DD".

## Mergulho Profundo:

A conversão de datas para strings é uma prática comum que remonta aos primórdios da programação. É uma maneira elegante de lidar com datas que precisam ser apresentadas de maneira clara e concisa aos usuários ou armazenadas para referência futura.

Existem diversas formas de formatação de datas, como "DD/MM/YYYY", "MM-DD-YYYY", "YYYY/MM/DD", dependendo do uso pretendido ou normas regionais. Em Gleam, você pode especificar o formato que deseja quando faz a formatação.

Vale lembrar que a biblioteca "gleam/time" utiliza Posix timestamp para calcular a data atual. Isso significa que se baseia em segundos a partir da meia-noite UTC do dia 1 de janeiro de 1970, um método de cálculo universalmente reconhecido.

## Veja Também:

Além do método acima, você também pode querer explorar a biblioteca "gleam/calendar" para possíveis alternativas. Também vale a pena conferir a documentação oficial do Gleam para mais detalhes e técnicas relacionadas: 

1. [Biblioteca Gleam/time](https://hexdocs.pm/gleam_stdlib/gleam/time.html)
2. [Biblioteca Gleam/calendar](https://hexdocs.pm/gleam_stdlib/gleam/calendar.html)
3. [Documentação Oficial do Gleam](https://gleam.run/docs/introduction/)