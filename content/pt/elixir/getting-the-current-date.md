---
title:    "Elixir: Obtendo a data atual"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar a programação Elixir?

Muitos programadores estão se rendendo à linguagem Elixir por sua simplicidade, performance e facilidade de aprendizado. Com uma sintaxe voltada para a funcionalidade e uma poderosa máquina virtual, ela é capaz de rodar em múltiplos processadores, o que garante que as aplicações escritas nesta linguagem rodem com rapidez e sem falhas.

## Como obter a data atual em Elixir

Para obter a data atual em Elixir, você pode utilizar a função `Date.utc_today`, que retorna a data atual no formato {year, month, day}. Veja abaixo um exemplo de código utilizando esta função:

```
Elixir -.Date.utc_today()
```

O output deste código será algo parecido com `{2020, 6, 30}`, representando a data de hoje.

Outra opção é utilizar a função `DateTime.utc_now`, que retorna a data e hora atual no formato `{year, month, day, hour, minute, second}`. Veja o exemplo abaixo:

```
Elixir - DateTime.utc_now()
```

Neste caso, o output será algo semelhante a `{2020, 6, 30, 8, 30, 15}`, representando a data e hora atual.

## Mergulhando mais fundo

Caso você precise de mais informações sobre a data, como dia da semana, fuso horário, entre outros, existem também outras funções disponíveis em Elixir. Por exemplo, a função `DateTime.utc_now` possui um parâmetro opcional `:microsecond`, que permite obter a data com mais precisão, incluindo os microssegundos.

Também é possível converter esse formato de data para um formato humano legível, utilizando a função `DateTime.to_iso8601`, que retorna uma string no formato `YYYY-MM-DD` ou `YYYY-MM-DDThh:mm:ssZ`. Isso pode ser especialmente útil para integrar com outras aplicações.

## Veja também

- [Documentação oficial do módulo Date em Elixir](https://hexdocs.pm/elixir/Date.html)
- [Sobre a linguagem Elixir](https://elixir-lang.org/)
- [Artigo sobre as vantagens da programação funcional](https://medium.com/@eduardomoroni/porque-usar-programa%C3%A7%C3%A3o-funcional-8baaff99a789)