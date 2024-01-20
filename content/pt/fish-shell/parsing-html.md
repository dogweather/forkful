---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Analisar (parse) HTML é o processo de decifrar o código das páginas da web, de modo a extrair e manipular informações. Programadores fazem isso para rastrear dados da web, testar a integridade do código, renderizar páginas na web, entre outros.

## Como Fazer:
Vamos parsear HTML com o Fish Shell (versão atual) usando o 'Beautiful Soup'.
Primeiro instale o lxml e o Beautiful Soup com o pip:
```Fish Shell
pip install beautifulsoup4 lxml
```
Agora, usamos o Beautiful Soup para analisar o HTML:
```Fish Shell
echo '
import sys
from bs4 import BeautifulSoup
html = """<html><head><title>Test Page</title></head><body><p>This is a test</p></body></html>"""
soup = BeautifulSoup(html, "lxml")
print(soup.prettify())
' | python
```
O output mostra a estrutura HTML analisada:
```HTML
<html>
 <head>
  <title>
    Test Page
  </title>
 </head>
 <body>
  <p>
   This is a test
  </p>
 </body>
</html>
```

## Mergulho Profundo
1. Contexto Histórico: O parsing de HTML tem sido usado desde o início dos tempos da web, permitindo que os navegadores convertam código em páginas renderizadas.
2. Alternativas: Além do Beautiful Soup, existem outras ferramentas para parsing de HTML, como o html.parser, lxml, html5lib e PyQuery.
3. Detalhes de Implementação: O Beautiful Soup permite parsear HTML e XML, ele situa cada elemento HTML como um objeto Python, o que facilita a manipulação do seu conteúdo.

## Veja Também
1. Beautiful Soup Documentação: [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
2. Como usar BeautifulSoup: [https://realpython.com/beautiful-soup-web-scraper-python/](https://realpython.com/beautiful-soup-web-scraper-python/)