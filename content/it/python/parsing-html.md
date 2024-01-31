---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:33:25.281370-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Parsare l'HTML significa analizzare il codice HTML per estrarne dati specifici. I programmatori lo fanno per automatizzare la raccolta di informazioni da siti web, un processo noto come "web scraping".

## How to:
Per parsare l'HTML in Python, possiamo usare la libreria `BeautifulSoup`. Ecco come:

```Python
from bs4 import BeautifulSoup
import requests

# Richiediamo il contenuto della pagina
url = 'https://example.com'
pagina = requests.get(url)

# Creiamo un oggetto BeautifulSoup
soup = BeautifulSoup(pagina.content, 'html.parser')

# Cerchiamo un elemento specifico
titolo = soup.find('h1').text

print(f'Il titolo della pagina è: {titolo}')
```

Output:
```
Il titolo della pagina è: Esempio di Titolo
```

## Deep Dive:
Parsare l'HTML non è una novità: è una pratica comune da quando il web è iniziato. Prima delle moderne librerie Python come `BeautifulSoup` e `lxml`, gli sviluppatori spesso si affidavano a espressioni regolari (regex), ma questo metodo può essere complicato e meno affidabile.

Alternative alla BeautifulSoup includono `lxml` e `html.parser` per Python, che possono essere più veloci in specifici casi d'uso, ma anche più complesse da gestire per i principianti.

È importante considerare anche le questioni legali ed etiche. Non tutti i siti web permettono il web scraping, quindi è fondamentale controllare i termini di servizio e utilizzare le API pubbliche quando possibile.

## See Also:
- Documentazione BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Documentazione `lxml`: https://lxml.de/
- Linee guida per scraping etico: https://www.promptcloud.com/blog/ethical-web-scraping/
- Web scraping con Python – Guida di Real Python: https://realpython.com/beautiful-soup-web-scraper-python/
