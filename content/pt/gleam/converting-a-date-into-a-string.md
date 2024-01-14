---
title:    "Gleam: Convertendo uma data em uma string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

Conversão de data em string é uma tarefa comum em programação, pois permite que dados sejam armazenados e exibidos em um formato legível para o usuário. Neste post, vamos discutir como fazer isso em Gleam, uma linguagem de programação funcional e estática.

## Como fazer

Para converter uma data em string em Gleam, podemos usar a função `format` do módulo `gleam/time`. Por exemplo, se quisermos exibir a data atual no formato "dd/mm/aaaa", podemos fazer o seguinte:

```gleam
import gleam/time

let today = time.now()
let formatted_date = time.format(today, "{DD}/{MM}/{YYYY}")
```

Aqui, estamos importando o módulo `gleam/time` e usando a função `now()` para obter a data e hora atuais. Em seguida, usamos o formato desejado dentro do segundo argumento da função `format()`, onde `{DD}` representa o dia, `{MM}` o mês e `{YYYY}` o ano.

Podemos até mesmo alterar o idioma da data usando o argumento opcional `locale`, como `{DD} de {MM} de {YYYY}`, que exibirá a data no formato "dd de mm de aaaa" (por exemplo, 17 de agosto de 2021).

## Mergulho Profundo

A função `format()` aceita diversos formatos para a data e hora, como `{HH}:{MM}:{SS}` para horas, minutos e segundos, `{hh}:{mm}:{ss} {a}` para horas no formato de 12 horas com "am" ou "pm", entre outros. Também é possível adicionar outros argumentos, como `{DDD}` para o dia da semana abreviado em inglês (por exemplo, "Mon", "Tue", etc.).

Além disso, podemos converter uma data em timestamp usando a função `timestamp()`, e depois convertê-lo em uma string usando a função `format()`.

## Veja também

- Documentação oficial do módulo `gleam/time`: https://gleam.run/modules/gleam_stdlib/time.html
- Tutorial sobre conversão de data em string em Gleam: https://medium.com/@rubysmith/converting-dates-to-strings-in-gleam-5c5c8824fe04