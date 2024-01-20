---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Scaricare una pagina web è assegnare a un tuo programma il compito di richiedere e ricevere dati da un sito web particolare. I programmatori fanno questo per vari motivi, ad esempio per analizzare il contenuto di una pagina, rilevare alterazioni o implementare dei bot.

## Come si fa:
Ecco un modo semplice per farlo usando la libreria `requests` in Python.

```Python
import requests

url = 'http://www.esempio.it'
risposta = requests.get(url)

print(risposta.text)
```
Proprio così, puoi scaricare una pagina web in soli tre righe di codice! 

## Approfondimento
Negli anni '90, scaricare una pagina web richiedeva codice complesso e non aveva molta utilità. Oggi, con l'aumento delle API web RESTful e dei servizi basati su cloud, scaricare una pagina web è diventato un compito comune. 

Ci sono molte alternative a `requests` come `urllib` e `httplib`, ma `requests` è generalmente preferita per la sua semplicità. 

Dal punto di vista dell'implementazione, quando invii una richiesta GET utilizzando `requests.get()`, stai chiedendo al server di inviarti dei dati. Il server risponde inviandoti il codice HTML della pagina, che viene memorizzato nella variabile `response.text`.

## Vedi anche
- [Documentazione ufficiale della libreria requests](https://docs.python-requests.org/)
- [Come scaricare una pagina web con Python e Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/#making-the-soup)
- [HTTP per principianti](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)