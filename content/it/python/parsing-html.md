---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

L'analisi del HTML, cioè il Parsing, è una pratica comune per ottenere informazioni specifiche da pagine web. I programmatori lo utilizzano per accedere e manipolare dati estratti da pagine HTML.

## Come fare:

L'uso di BeautifulSoup è un modo comodo per effettuare l'analisi del HTML. Qui un piccolo exemplo di codice per estrarre i titoli di un blog.

```Python
from bs4 import BeautifulSoup
import requests

response = requests.get("http://www.eblog.com")
soup = BeautifulSoup(response.text, 'html.parser')

for title in soup.find_all('h2'):
    print(title.text)
```

Nel caso in cui l'URL del blog sia "http://www.eblog.com", l'output mostrerebbe tutti i titoli degli articoli presenti su quella pagina.

## Approfondimento

L'analisi del HTML risale alle prime fasi dello sviluppo del web, quando i dati erano incorporati principalmente in pagine HTML. Oggi, esistono diverse alternative come l'uso di API o di Web scraping a livello di browser. Tuttavia, l'analisi del HTML rimane un'eccellente opzione per i dati contenuti in pagine web statiche.

Alcune librerie, oltre a BeautifulSoup, sono utilizzate per l'analisi del HTML in Python. Queste includono lxml e html.parser, entrambe con vantaggi e svantaggi specifici.

## Vedere anche:

1. Documentazione di BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
2. Guida a Lxml: https://lxml.de/tutorial.html
3. Documentazione di Html.parser: https://docs.python.org/3/library/html.parser.html
4. Scrapy: https://scrapy.org/
5. Selenium: https://www.selenium.dev/