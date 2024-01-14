---
title:                "Python: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'abilità essenziale per tutti i programmatori Python che lavorano con dati da internet. Questa abilità consente di accedere e analizzare informazioni provenienti da fonti online.

## Come fare

Per scaricare una pagina web in Python, esistono diverse opzioni. Una delle più comuni è utilizzare la libreria `requests`, che permette di effettuare richieste HTTP in modo semplice ed efficiente. Vediamo un esempio di codice per scaricare la pagina principale di Google:

```python
import requests

response = requests.get("https://www.google.com")

print(response.status_code)
print(response.content)
```

Il codice sopra fa prima un `import` della libreria `requests`. Poi è usata la funzione `get()` per effettuare una richiesta all'URL specificato. Infine, viene stampato il codice di stato della risposta (che dovrebbe essere 200 se la richiesta è andata a buon fine) e il contenuto della pagina web, che viene restituito come una stringa di byte.

## Approfondimento

Mentre il codice di esempio funziona semplicemente per una pagina come quella di Google, scaricare pagine web può diventare più complicato quando si incontrano situazioni come autenticazione, header personalizzati o richieste POST. Per questo, è sempre bene consultare la documentazione della libreria `requests` o ricorrere ad altre alternative come `urllib` o `httplib`.

## Vedi anche

- [Documentazione di requests](https://2.python-requests.org/)
- [Gestione degli errori in Python](https://realpython.com/python-exceptions/)
- [Un'introduzione a scrivere script Python efficaci](https://realpython.com/python-scripts/)