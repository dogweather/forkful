---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair dados de HTML, ou parsing HTML, é o processo de analisar o código HTML para extrair informações específicas. Essa tarefa é frequentemente usada por programadores quando eles precisam interagir com websites e analisar os seus conteúdos.

## Como Fazer:
O módulo `Floki` é um dos pacotes mais populares para processar HTML em Elixir. Ele também possui uma sintaxe CSS para selecionar elementos HTML. Vamos fazer uma tentativa:

Para instalar o Floki, adicione a seguinte linha no seu arquivo mix.exs:
```Elixir
defp deps do
  [
    {:floki, "~> 0.30"}
  ]
end
```
Agora você pode usá-lo para extrair dados de um documento HTML:
```Elixir
html = "<div><p>Hello, World!</p></div>"
{:ok, document} = Floki.parse_document(html)
Floki.find(document, "p")
# => [{"p", [], ["Hello, World!"]}]
```
Neste exemplo, extraímos o conteúdo do elemento `<p>` do HTML.

## Aprofundando
Extrair dados de HTML não é uma prática nova. Historicamente, bibliotecas como BeautifulSoup em Python têm servido a esse propósito. Em Elixir, além de Floki, existem outras alternativas como o `Meeseeks` e o `Crawly`.

Floki usa uma implementação baseada em árvore, lidando com o HTML como uma árvore de nós onde cada tag é representada como uma tupla Elixir. O Floki pode identificar, percorrer e modificar estas árvores de nós de uma maneira eficiente.

Além disso, Floki é amplamente implementado em Elixir puro, o que lhe permite aproveitar a alta concorrência e tolerância a falhas fornecidas pelo Elixir e pela máquina virtual Erlang.

## Veja Também
1. Documentação do Floki: https://hexdocs.pm/floki/readme.html
2. Repositório do GitHub para Meeseeks: https://github.com/mischov/meeseeks
3. Documentação do Crawly: https://hexdocs.pm/crawly/readme.html