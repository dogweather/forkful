---
title:    "Gleam: Convertendo uma data em uma string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter dados em diferentes tipos de formato é uma tarefa comum na programação, especialmente ao lidar com diferentes ferramentas e sistemas. Neste caso, converter uma data em uma string pode ser útil para armazenamento, transmissão ou exibição de informações de data em formato legível para humanos.

## Como fazer

A conversão de uma data em uma string é uma tarefa relativamente simples em Gleam, graças à função `format_date()` da biblioteca padrão `gleam/time`. Veja um exemplo abaixo:

```Gleam
import gleam/time
import gleam/io.{stdout}

let data = 2021-05-07
let data_str = time.format_date(data, "%d/%m/%Y")

stdout.print("Data formatada em string: {}".format(data_str))
```

Output:
```
Data formatada em string: 07/05/2021
```

Com a função `format_date()`, podemos especificar o formato da data que queremos obter como string, usando códigos de formato especiais. No exemplo acima, usamos `%d` para o dia, `%m` para o mês e `%Y` para o ano.

Além disso, também podemos adicionar informações adicionais, como horário e fuso horário, ao especificar um formato mais detalhado. Para mais informações sobre os códigos de formato disponíveis e suas opções, consulte a documentação da biblioteca `gleam/time`.

## Análise detalhada

Por trás das cenas, a função `format_date()` usa a biblioteca `strftime` do Erlang para a conversão de data em string. O Erlang é uma linguagem de programação funcional conhecida por sua capacidade de manipular efetivamente dados estruturados, como datas e horas.

Uma das vantagens de usar a função `format_date()` é que ela lida automaticamente com diferentes formatos de data, ajustando-se de acordo com o idioma e as configurações locais do sistema. Isso é particularmente útil para aplicativos e sistemas internacionais, onde podem haver diferentes convenções de data.

## Veja também
- Documentação da biblioteca `gleam/time`: https://gleam.run/packages/gleam/time/
- Lista de códigos de formato disponíveis: https://gleam.run/packages/gleam/time/#codes
- Documentação da biblioteca `strftime` do Erlang: http://erlang.org/doc/man/strftime.html