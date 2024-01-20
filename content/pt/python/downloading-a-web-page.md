---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Baixar uma página da internet significa extrair o conteúdo HTML de um site utilizando algum programa. Os programadores fazem isso para analisar, alterar ou utilizar o conteúdo da página em seus próprios programas.

## Como fazer:

Para baixar uma página da web em Python, utilizamos o módulo 'requests'. Aqui está um exemplo simples:

```Python
import requests

url = "https://www.google.com"
req = requests.get(url)

print(req.text)
```

Quando executar esse código, você receberá como saída todo o conteúdo HTML da página inicial do Google.

## Mergulho Profundo

O ato de baixar páginas da web tem suas origens no início da internet, quando os navegadores de internet precisavam exibir o conteúdo de um site. Hoje em dia, isso é especialmente útil para a mineração de dados da web ou para criar bots.

Você pode também utilizar bibliotecas alternativas como urllib, httplib, treq, etc. Cada uma tem suas próprias vantagens e peculiaridades.

Ao baixar uma página da web, em muitos casos é importante lidar com as respostas de status HTTP. Por exemplo, o código 200 significa 'sucesso', enquanto que o código 404 indica que a página não foi encontrada.

## Veja Também

Se você quiser aprofundar seus conhecimentos em Python ou na Web Scraping, aqui estão alguns links úteis:

- W3Schools Python Tutorial: https://www.w3schools.com/python/
- Scrapy (uma biblioteca Python para web scraping): https://scrapy.org/
- Tutorial de requests do Python: https://requests.readthedocs.io/
- Documentação oficial do Python: https://docs.python.org/pt-br/3/