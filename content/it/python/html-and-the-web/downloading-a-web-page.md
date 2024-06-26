---
date: 2024-01-20 17:44:42.541235-07:00
description: "How to: Python rende il download di pagine web un gioco da ragazzi.\
  \ Un modo semplice \xE8 usare `requests`. Ecco come."
lastmod: '2024-03-13T22:44:42.999644-06:00'
model: gpt-4-1106-preview
summary: Python rende il download di pagine web un gioco da ragazzi.
title: Scaricare una pagina web
weight: 42
---

## How to:
Python rende il download di pagine web un gioco da ragazzi. Un modo semplice è usare `requests`. Ecco come:

```Python
import requests

url = 'https://www.esempio.com'
risposta = requests.get(url)

if risposta.status_code == 200:
    contenuto = risposta.text
    print(contenuto[:200]) # stampa i primi 200 caratteri
else:
    print(f"Errore: {risposta.status_code}")
```

Output (truncated per demo):
```Python
<!DOCTYPE html>
<html lang="it">
<head>
    <title>Esempio di Pagina Web</title>
...
# eccetera
```
Ricorda di gestire le eccezioni e gli errori nella rete!

## Deep Dive
Una volta, il download delle pagine web era un affare molto manuale. Oggi, librerie come `requests` in Python o `curl` in bash script semplificano tutto. Altri strumenti Python includono `urllib` e `http.client`, ma `requests` è più semplice.

`requests` implementa i metodi HTTP per farti comunicare con i server web facilmente. Rispetto ad `urllib`, `requests` ha un API più user-friendly ed è idonea alla maggior parte delle esigenze.

Attenzione però ai termini di servizio dei siti – alcuni proibiscono lo scraping o l'accesso automatizzato. E poi c'è l'intestazione `User-Agent`: cambiarla può aiutarti a non essere bloccato dai server che cercano di bloccare i bot.

## See Also
Guarda anche questi:

- Documentazione di `requests`: https://requests.readthedocs.io/it/latest/
- Beautiful Soup per l'analisi HTML: 
  https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Documentazione di `urllib`: https://docs.python.org/3/library/urllib.html

E non dimenticare di verificare la legalità dei tuoi scraping e del download automatico. Buona programmazione!
