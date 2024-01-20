---
title:                "Análise de HTML"
date:                  2024-01-20T15:31:00.134803-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Parsear HTML é o processo de ler e entender o código HTML, tipicamente para extrair informações dele. Programadores fazem isso para interagir com dados de páginas web, coletar dados para análise, ou até automatizar tarefas na web.

## Como Fazer:

```elixir
# Adiciona Floki ao mix.exs
defp deps do
  [{:floki, "~> 0.31.0"}]
end

# Executa o `mix deps.get` para instalar

# Exemplo simples de parsing HTML com Floki
defmodule HTMLParser do
  def parse(html) do
    {:ok, document} = Floki.parse_document(html)
    Floki.find(document, "h1") |> Floki.text()
  end
end

# Uso:
html_content = "<html><body><h1>Olá, Elixir!</h1></body></html>"
resultado = HTMLParser.parse(html_content)
IO.puts(resultado) # Saída: "Olá, Elixir!"
```

## Mergulho Profundo:

O ato de parsear HTML é fundamental na web desde seus primeiros dias. No passado, outras linguagens como PHP e Python eram as mais comuns para essa tarefa, mas hoje Elixir oferece uma abordagem moderna e eficiente através de bibliotecas como Floki, que faz uso do parser HTML5.

Alternativas para Floki incluem outras bibliotecas como MochiWeb e Phoenix.HTML, cada uma com suas particularidades e casos de uso específicos. Floki se destaca pela sintaxe simples e pela inspiração no jQuery para seleçoes do DOM.

Detalhes de implementação interessantes incluem o uso de expressões XPath ou seletores CSS para navegar pelo DOM (Documento Object Model) do HTML, e as particularidades do parsing HTML5, que é mais complexo que XML devido às suas 'quirks modes' e auto-correções de erros nos browsers.

## Veja Também:

- Documentação do Floki: [https://hexdocs.pm/floki](https://hexdocs.pm/floki)
- Elixir School (em Português): [https://elixirschool.com/pt](https://elixirschool.com/pt)