---
title:                "Scaricare una pagina web"
html_title:           "Python: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché
Se sei interessato allo sviluppo web o al data scraping, potresti essere curioso di conoscere come scaricare una pagina web utilizzando Python.

## Come
Per scaricare una pagina web utilizzando Python, c'è bisogno di due librerie: `requests` e `urllib`. Per prima cosa, importa le librerie nel tuo codice:

```Python
import requests
import urllib.request
```

Quindi, puoi utilizzare il metodo `get()` dalla libreria `requests` per scaricare il contenuto della pagina web specificando l'URL come argomento:

```Python
url = "https://www.example.com"
web_page = requests.get(url)
```

Ora puoi accedere al contenuto della pagina utilizzando l'attributo `.text`:

```Python
print(web_page.text)
```

In alternativa, puoi utilizzare la libreria `urllib` e il metodo `urlopen()` per scaricare la pagina web nello stesso modo:

```Python
url = "https://www.example.com"
web_page = urllib.request.urlopen(url)
```

Anche in questo caso, puoi accedere al contenuto della pagina utilizzando l'attributo `.read()`:

```Python
print(web_page.read())
```

## Deep Dive
In entrambi i casi, utilizzando `requests` o `urllib`, è possibile specificare altri parametri come header, cookies o autenticazione per personalizzare la richiesta. Inoltre, è possibile gestire gli errori utilizzando il try-except block e scaricare il contenuto in diversi formati utilizzando il metodo `json()` da `requests` o il modulo `json` integrato da `urllib`.

## Vedi anche
- Documentazione su `requests`: https://requests.readthedocs.io/en/latest/
- Documentazione su `urllib`: https://docs.python.org/3/library/urllib.html