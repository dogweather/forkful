---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:57.818892-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Obter a data atual no Elixir é simplesmente pegar a data do momento em que o código é executado. Programadores fazem isso para registrar quando eventos ocorrem, executar funções baseadas em datas ou simplesmente apresentar datas aos usuários.

## Como Fazer:
```elixir
# Para pegar a data atual no Elixir, você pode usar o módulo Date.
date = Date.utc_today()
IO.inspect(date)

# Saida esperada (depende do dia em que você rodar o código):
# ~D[2023-04-01]
```

## Mergulho Profundo
O Elixir, assim como outras linguagens funcionais modernas, possui uma biblioteca padrão rica, o que inclui manipulação de datas e tempo. Uma alternativa ao `Date.utc_today/0` é usar o `DateTime`, que também fornece informações sobre o horário e fuso horário, oferecendo maior precisão se necessário. A implementação dessas funções depende do calendário ISO 8601, adotado como padrão desde 1988 devido à sua ampla aceitação e a facilidade de implementação em sistemas computacionais.

## Veja Também
- [Documentação oficial do Módulo Date](https://hexdocs.pm/elixir/Date.html)
- [Documentação oficial do Módulo DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601 na Wikipedia](https://pt.wikipedia.org/wiki/ISO_8601)
