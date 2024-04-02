---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:44.583237-07:00
description: "L'analisi (parsing) di HTML comporta l'analisi del codice HTML di una\
  \ pagina web per estrarre informazioni o elementi specifici, un compito comune per\
  \ lo\u2026"
lastmod: '2024-03-13T22:44:42.998704-06:00'
model: gpt-4-0125-preview
summary: "L'analisi (parsing) di HTML comporta l'analisi del codice HTML di una pagina\
  \ web per estrarre informazioni o elementi specifici, un compito comune per lo\u2026"
title: Analisi del HTML
weight: 43
---

## Cosa & Perché?
L'analisi (parsing) di HTML comporta l'analisi del codice HTML di una pagina web per estrarre informazioni o elementi specifici, un compito comune per lo scraping del web, il data mining o l'automatizzazione delle interazioni con i siti web. I programmatori lo fanno per interagire programmaticamente con i siti web o estrarre dati da essi, automatizzare compiti o testare applicazioni web.

## Come fare:
Python offre potenti librerie come BeautifulSoup e requests per lo scraping del web e l'analisi di HTML. Per iniziare, è necessario installare queste librerie se non lo avete già fatto:

```bash
pip install beautifulsoup4 requests
```

Ecco un esempio basilare che utilizza `requests` per recuperare il contenuto HTML di una pagina web e `BeautifulSoup` per analizzarlo:

```python
import requests
from bs4 import BeautifulSoup

# Recupera il contenuto di una pagina web
URL = 'https://example.com'
page = requests.get(URL)

# Analizza il contenuto HTML
soup = BeautifulSoup(page.content, 'html.parser')

# Esempio di estrazione del titolo della pagina web
title = soup.find('title').text
print(f'Titolo della pagina web: {title}')
```

**Output dell'esempio**:
```
Titolo della pagina web: Esempio di Dominio
```

Per query più complesse, come l'estrazione di tutti i link da una pagina web, è possibile utilizzare i vari metodi di BeautifulSoup per navigare e cercare nell'albero di analisi:

```python
# Estrai tutti i link contenuti nei tag <a>
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Output dell'esempio**:
```
https://www.iana.org/domains/example
```

La flessibilità di BeautifulSoup consente di personalizzare la ricerca per i dati esatti di cui si ha bisogno, rendendo l'analisi di HTML uno strumento potente per i programmatori che lavorano con i contenuti web.
