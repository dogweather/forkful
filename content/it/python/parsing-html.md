---
title:                "Python: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Parlare di HTML può sembrare noioso per alcuni, ma saper estrarre e manipolare i dati da una pagina web può essere un'abilità molto utile in molte situazioni. Con il parsing HTML, è possibile ottenere informazioni dettagliate da diversi siti web, automatizzare processi e creare applicazioni web.

## Come Fare

Per iniziare, è necessario avere familiarità con il linguaggio di programmazione Python e anche con il formato HTML delle pagine web. In Python, esistono diverse librerie per il parsing HTML, ma qui utilizzeremo `BeautifulSoup` per la sua semplicità.

Per prima cosa, importiamo `BeautifulSoup` nel nostro codice:

```Python
from bs4 import BeautifulSoup
```

Successivamente, dobbiamo scaricare il contenuto della pagina web su cui vogliamo effettuare il parsing. Possiamo farlo utilizzando la libreria di `requests` e in seguito utilizzando il metodo `get()` per ottenere il contenuto della pagina web.

```Python
import requests
response = requests.get("https://www.example.com")
```

Ora, abbiamo bisogno di convertire il contenuto della pagina web in un formato che `BeautifulSoup` possa leggere. Possiamo farlo utilizzando il parser `html.parser` fornito con la libreria.

```Python
page_content = BeautifulSoup(response.text, 'html.parser')
```

Una volta ottenuto il contenuto, possiamo iniziare a leggere e manipolare i dati all'interno della pagina. Utilizzeremo le funzioni di `BeautifulSoup` per individuare elementi specifici all'interno dello stesso.

Ad esempio, se volessimo ottenere il contenuto di un tag ` <h1> ` all'interno della pagina:

```Python 
title = page_content.find("h1")

# output: <h1>Benvenuti nel mio sito web</h1>
```

Possiamo anche estrarre il contenuto del tag utilizzando il metodo `get_text()`:

```Python 
title = page_content.find("h1").get_text()

# output: Benvenuti nel mio sito web
```

## Deep Dive

Il parsing HTML diventa più interessante quando si utilizzano pagine web con contenuti dinamici o dati costantemente aggiornati. In questi casi, è possibile utilizzare librerie come `Selenium` per automatizzare l'apertura e la navigazione all'interno della pagina web.

Inoltre, è possibile utilizzare strumenti come `XPath` per selezionare elementi specifici all'interno della pagina in base al loro percorso gerarchico.

## Vedi Anche

- Documentazione di BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Documentazione di Selenium: https://selenium-python.readthedocs.io/
- Guida a XPath: https://www.w3schools.com/xml/xpath_intro.asp