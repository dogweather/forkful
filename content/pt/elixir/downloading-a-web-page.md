---
title:                "Baixando uma página da web"
html_title:           "Elixir: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

O que e por que?

Baixar uma página web é quando o código é usado para acessar e salvar uma página da internet para uso posterior. Os programadores fazem isso para obter informações de uma página ou para automatizar tarefas.

Como fazer:

```
Elixir HTTPoison.get("www.exemplo.com")
```

Saída:
```
{:ok, %HTTPoison.Response{...}}
```

Deep Dive:

Baixar páginas web tem sido uma prática comum na programação de aplicativos por várias décadas. Existem várias ferramentas e linguagens que podem ser usadas para fazer isso, como Python, Java e Ruby. No entanto, o Elixir oferece uma maneira simples e eficiente de fazer isso com a biblioteca HTTPoison.

Veja também:

- Documentação oficial do Elixir - https://elixir-lang.org/
- HTTPoison no Github - https://github.com/edgurgel/httpoison