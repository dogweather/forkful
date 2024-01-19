---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---

# Analisando datas em Gleam: o quê e por quê?

Analisar datas a partir de uma string é o ato de interpretar e converter sequências de caracteres num formato de data legível por programas. Programadores fazem isso para manipular e trabalhar com dados de tempo de maneira mais eficaz, direta e humanamente compreensível.

## Como fazer:

Gleam ainda não tem bibliotecas integradas para análise de datas, então vamos criar uma função simples para esse fim. Aqui nós a chamamos de `parse_date`, que receberá uma string e retornará uma tupla.

```gleam
fn parse_date(date_string: String) -> Result(tuple(String, String, String), Nil) { 
    case String.split(date_string, "/") {
        [Ok(day), Ok(month), Ok(year)] -> 
            Ok(tuple(day, month, year))
            
        _ -> 
            Error(Nil)
    }
}
```

Após chamar `parse_date("10/11/2020")`, você receberá `Ok(tuple("10", "11", "2020"))`.

## Mergulho profundo

Historicamente, analisar datas é um problema comum na maioria das linguagens de programação. Para resolver isso, as linguagens modernas, como Python e Java, têm bibliotecas poderosas incorporadas para lidar com datas e horários.

Embora Gleam ainda não tenha um pacote de data e hora robusto, você pode usar funções personalizadas ou wrappers em torno das bibliotecas Erlang para analisar datas como no exemplo acima.

Existem alguns pacotes Erlang poderosos como o `calendar` e o `erlang` disponíveis para uso, embora eles possam não ser tão idiomáticos para usuários Gleam e possam exigir um pouco mais de trabalho para serem configurados corretamente.

## Veja também

1. A documentação oficial do Gleam: https://gleam.run/docs/
2. Exemplos do Gleam: https://github.com/gleam-lang/example
3. Calendário e bibliotecas Erlang: http://erlang.org/doc/man/calendar.html.