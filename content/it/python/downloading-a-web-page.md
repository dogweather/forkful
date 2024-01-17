---
title:                "Scaricando una pagina web"
html_title:           "Python: Scaricando una pagina web"
simple_title:         "Scaricando una pagina web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il download di una pagina web si riferisce al processo di scaricare il contenuto di una pagina da Internet. I programmatori lo fanno per ottenere informazioni da un sito web o per analizzare il codice sorgente di una pagina.

## Come fare:
```Python
# Importa il modulo "requests"
import requests

# Utilizza il metodo "get" per scaricare il contenuto di una pagina web
response = requests.get("https://www.esempio.com")

# Stampa il contenuto della pagina
print(response.text)
```

L'esempio sopra utilizza il modulo "requests" per scaricare il contenuto della pagina web e stamparlo. È possibile anche specificare l'indirizzo IP della pagina anziché l'URL.

## Approfondimento:
Ci sono diversi modi per scaricare una pagina web in Python. Oltre al modulo "requests", è possibile utilizzare anche i moduli "urllib" e "urllib2". Inoltre, esistono diverse librerie di scraping come "Beautiful Soup" che semplificano il processo di estrazione delle informazioni da una pagina web.

È importante notare che il download di una pagina web senza il permesso del proprietario potrebbe essere considerato una violazione dei diritti d'autore. Inoltre, alcune pagine web potrebbero impostare delle restrizioni per prevenire il download del loro contenuto.

## Vedi anche:
- Documentazione del modulo "requests": https://requests.readthedocs.io/en/master/
- Documentazione del modulo "urllib": https://docs.python.org/3/library/urllib.html
- Documentazione del modulo "urllib2": https://docs.python.org/3/library/urllib2.html
- Libreria "Beautiful Soup" per lo scraping: https://www.crummy.com/software/BeautifulSoup/
- Leggi sulla proprietà intellettuale e il web scraping: https://developer.ibm.com/open/ebooks/legal-guides-on-web-scraping-and-artificial-intelligence/
- Esempi di codice per il download di una pagina web con diverse librerie: https://www.bogotobogo.com/python/python_network_programming_server_client_download.php