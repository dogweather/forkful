---
date: 2024-01-20 17:36:34.698187-07:00
description: "Converter uma data em uma string \xE9 basicamente transformar o formato\
  \ de data, normalmente estruturado e reconhecido pelo computador, em texto leg\xED\
  vel por\u2026"
lastmod: 2024-02-19 22:05:05.325863
model: gpt-4-1106-preview
summary: "Converter uma data em uma string \xE9 basicamente transformar o formato\
  \ de data, normalmente estruturado e reconhecido pelo computador, em texto leg\xED\
  vel por\u2026"
title: Convertendo uma data em uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data em uma string é basicamente transformar o formato de data, normalmente estruturado e reconhecido pelo computador, em texto legível por humanos. Programadores fazem isso para exibir datas em um formato específico, facilitando a interação e compreensão por usuários ou para registrar em arquivos de log.

## Como Fazer:
```elixir
defmodule DateConverter do
  def to_string(date) do
    date
    |> Date.to_iso8601()
  end
end

# Uso:
IO.puts DateConverter.to_string(~D[2023-04-12])
```
Saída de exemplo:
```
"2023-04-12"
```

## Aprofundando
Conversão de datas para strings não é novidade. Em Elixir, a capacidade de manipular datas veio após algumas versões, sendo refinada ao longo do tempo. Antes de ter módulos específicos como `Date`, programadores dependiam de bibliotecas externas ou manipulações manuais de strings.

Alternativamente, além de `Date.to_iso8601/1`, que converte para o padrão ISO 8601, Elixir também oferece outras funções como `Date.to_string/1`, que foi preterida, mas ainda pode ser encontrada em código antigo.

Os detalhes de implementação frequentemente dependem do que se pretende com o formato final da data. Por exemplo, para registrar em logs pode-se querer um formato completo com tempo até milissegundos, enquanto para exibir para usuários finais prefere-se algo mais legível, talvez até regionalizado.

## Veja Também
- [Elixir Date](https://hexdocs.pm/elixir/Date.html) – Documentação oficial dos módulos de data em Elixir.
- [Elixir DateTime](https://hexdocs.pm/elixir/DateTime.html) – Para uma manipulação mais completa de datas e tempos.
- [Elixir Timex](https://hex.pm/packages/timex) – Uma biblioteca completa para trabalhar com datas e horas em Elixir, caso precise de funções mais avançadas.
