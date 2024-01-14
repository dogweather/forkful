---
title:                "Elixir: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Neste post, vamos explorar como usar a linguagem de programação Elixir para analisar HTML. Esta habilidade é útil quando você precisa extrair dados de uma página da web ou criar um web crawler para coletar informações.

## Como Fazer

Antes de começar, certifique-se de ter instalado o Elixir em seu sistema. Em seguida, vamos precisar de um pacote de terceiros chamado "Floki" para facilitar a análise HTML. Você pode instalá-lo usando o gerenciador de pacotes do Elixir, o Hex. No terminal, execute o seguinte comando:

```
mix deps.get floki
```

Agora que temos o Floki instalado, vamos começar com um exemplo simples de como analisar um HTML básico. Usando o módulo `Floki`, podemos usar a função `parse/1` e passar o HTML como uma string. Em seguida, usamos a função `Floki.find/2` para encontrar os elementos desejados.

```
# Usando o módulo Floki
html = "<div id="content"><h1>Título<h1><p>Este é um exemplo de parágrafo</p></div>"
parsed = Floki.parse(html)

# Encontrando o título
title = Floki.find(parsed, "h1")
IO.puts(title)

# Encontrando o parágrafo
paragraph = Floki.find(parsed, "p")
IO.puts(paragraph)
```

A saída será:

```elixir
[Título]
[Este é um exemplo de parágrafo]
```

## Aprofundando-se

O pacote Floki oferece várias funções úteis para análise e manipulação de HTML. Podemos usar a função `Floki.attribute/2` para encontrar o valor de um atributo específico em um elemento. Também podemos usar a função `Floki.html_to_text/1` para converter o HTML em texto simples. Aqui está um exemplo:

```
# Encontrando o href do link
link = Floki.find("#link")
href = Floki.attribute(link, "href")

# Convertendo o HTML em texto
text = Floki.html_to_text(html)
IO.puts(text)
```

Outra função útil é `Floki.matches?/2`, que verifica se um elemento corresponde a um seletor específico. Por exemplo, podemos verificar se o elemento é um h1 com a classe "title" da seguinte forma:

```
# Verificando se o elemento é um h1 com a classe "title"
Floki.matches?(title, "h1.title")
# Output: true
```

Com o pacote Floki, temos muitas ferramentas à nossa disposição para analisar e manipular HTML em nossos projetos Elixir.

## Veja Também

- Documentação do pacote Floki: [https://hexdocs.pm/floki/readme.html](https://hexdocs.pm/floki/readme.html)
- Tutorial Elixir: [https://elixir-lang.org/getting-started/introduction.html](https://elixir-lang.org/getting-started/introduction.html)
- Mais recursos sobre Elixir: [https://elixir-lang.org/resources.html](https://elixir-lang.org/resources.html)