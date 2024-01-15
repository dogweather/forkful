---
title:                "Analisi dell'html"
html_title:           "Python: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-html.md"
---

{{< edit_this_page >}}

## Perché
Il parsing di HTML è un'attività comune e utile per gli sviluppatori Python. Grazie alla sua flessibilità, è possibile estrarre facilmente dati strutturati da pagine web e utilizzarli per fini diversi come il web scraping, l'analisi dei dati o la creazione di applicazioni automatizzate.

## Come fare
Per iniziare a parsare HTML con Python, è necessario installare una libreria chiamata "BeautifulSoup". È possibile farlo utilizzando il comando `pip install beautifulsoup4` sul terminale. Una volta installata, importate la libreria nel vostro script di Python con `from bs4 import BeautifulSoup`.

Crea un'istanza di BeautifulSoup utilizzando il codice seguente:
```Python
from bs4 import BeautifulSoup
html = "<html><head><title>Titolo</title></head><body><p>Paragrafo</p></body></html>"
soup = BeautifulSoup(html, 'html.parser')
```

Per estrarre il titolo e il paragrafo dal codice HTML, utilizzate il seguente codice:
```Python
title = soup.find('title')
print(title.text) # stampa il testo del titolo: "Titolo"

paragraph = soup.find('p')
print(paragraph.text) # stampa il testo del paragrafo: "Paragrafo"
```

## Approfondimenti
BeautifulSoup dispone di diverse funzionalità per aiutarvi a navigare e estrarre dati da pagine HTML più complesse. Una di queste è il metodo `.find_all()`, che permette di trovare tutti gli elementi con una specifica etichetta HTML. Potete anche utilizzare selettori CSS per cercare elementi specifici all'interno della pagina.

Inoltre, Beautiful Soup supporta anche l'analisi di HTML non correttamente strutturato. Questo significa che potete estrarre dati da pagine web anche se presentano errori di codifica.

## Vedi anche
- Documentazione ufficiale di BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Tutorial su come parsare HTML con Python: https://realpython.com/beautiful-soup-web-scraper-python/
- Più esempi di utilizzo di Beautiful Soup: https://www.dataquest.io/blog/web-scraping-tutorial-python/