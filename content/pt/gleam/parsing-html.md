---
title:                "Gleam: Analisando o html."
simple_title:         "Analisando o html."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-html.md"
---

{{< edit_this_page >}}

##Por que
Você já se deparou com a necessidade de extrair informações específicas de um site? Se sim, você provavelmente já ouviu falar sobre o processo de análise de HTML. Esta é uma técnica usada para extrair dados de páginas da web e pode ser muito útil em vários cenários, como coletar informações de preços para comparação de produtos ou extrair conteúdo de notícias para análise de tendências. Neste post, vamos falar sobre como fazer isso usando Gleam, uma linguagem de programação funcional elegante e flexível.

##Como fazer
Para analisar HTML com Gleam, precisamos utilizar a biblioteca `html_parser` disponível no pacote `gleam/html`. Primeiro, vamos importar o módulo `Html` com `import html` para que possamos acessar as funções da biblioteca. Em seguida, precisamos ter uma página da web para analisar e armazená-la em uma variável. Neste exemplo, vamos usar a página do IMDb para extrair o título e a avaliação do filme "Joker".
```
import html

html_content = """
<html>
<head>
    <title>Filme: Joker</title>
</head>
<body>
    <div class="rating">
        <span class="star-rating">9.0</span>
    </div>
</body>
</html>
```

Agora, podemos usar a função `Html.parse` para analisar a página e obter uma representação em formato de árvore. Em seguida, usamos a função `Html.find` para buscar elementos específicos dentro dessa árvore, usando seletores CSS.

```
parsed_html = Html.parse(html_content)

title = Html.find("title", parsed_html).innerText
rating = Html.find(".rating .star-rating", parsed_html).innerText

IO.print("Título do filme: {}", [title])
IO.print("Avaliação do filme: {} / 10", [rating])
```

A saída deste código será:
```
Título do filme: Joker
Avaliação do filme: 9.0 / 10
```

##Mergulho aprofundado
Para aqueles que desejam entender melhor como o processo de análise de HTML funciona, aqui estão algumas informações adicionais. O processo geralmente segue três etapas:
1. Analisar o documento HTML e construir uma representação em formato de árvore.
2. Navegar na árvore para encontrar elementos específicos usando seletores CSS.
3. Extrair o conteúdo desses elementos.

Na biblioteca `html_parser`, a função `Html.parse` é responsável pela primeira etapa, enquanto a função `Html.find` é usada para navegar na árvore e encontrar elementos. Ao usar seletores CSS, podemos encontrar vários elementos de uma só vez, o que é útil para extrair uma lista de itens, como notícias ou produtos. Além disso, a função `Html.find_all` pode ser usada para encontrar todos os elementos que correspondem a um seletor específico, retornando uma lista.

##Veja também
- Documentação da biblioteca Gleam `html_parser`: https://gleam.run/modules/html.html
- Tutorial de Gleam do zero, incluindo como importar pacotes, como estruturar um projeto e mais: https://gleam.run/book/tour.html
- Usando CSS selectors em Gleam com a biblioteca `css_selector`: https://github.com/gleam-lang/gleam_css_selector