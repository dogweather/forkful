---
title:                "Scaricare una pagina web"
html_title:           "Gleam: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, probabilmente hai già usato la libreria standard di Erlang per scaricare una pagina web. Ma se vuoi un'esperienza più moderna e funzionale, allora è il momento di provare Gleam! Con una sintassi intuitiva e tipi di dati statici, Gleam rende semplice e affidabile il processo di download di pagine web.

## Come

Per iniziare, installa Gleam e il suo compilatore tramite il gestore di pacchetti esatto del tuo sistema operativo. Dopodiché, segue questi semplici passaggi in un file nuovo di Gleam:

```Gleam
import gleam/http
import gleam/io

response = http.get("https://www.example.com/")

io.print(response.body)
```

Il codice sopra importa i moduli di Gleam per il download delle pagine web e la stampa dei risultati. Dopo aver scaricato la pagina del nostro esempio, il codice stampa il corpo della risposta della pagina sul terminale. 

Ma cosa succede se vogliamo passare dei parametri alla nostra richiesta? Gleam rende anche questa operazione semplice:

```Gleam
import gleam/http
import gleam/io

url = "https://www.example.com/search"
params = [("q", "Gleam")]

response = http.get(url, params)

io.print(response.body)
```

In questa versione, invece di passare solo l'URL come argomento, passiamo anche una lista di coppie chiave-valore che rappresentano i parametri che vogliamo inviare nella richiesta. Verrà eseguito il download della pagina con i parametri specificati e il suo risultato verrà stampato sul terminale.

## Deep Dive

Ora che hai visto come eseguire il download di pagine web in modo semplice e diretto con Gleam, potresti chiederti come funziona il tutto dietro le quinte. In realtà, Gleam utilizza la libreria libcurl per gestire le richieste HTTP. Ciò significa che ha a disposizione un'ampia gamma di funzionalità avanzate per la gestione delle richieste, come le intestazioni personalizzate e gli agenti utente.

Inoltre, Gleam utilizza anche il sistema di tipi statico di Erlang per garantire che i dati scaricati siano gestiti in modo sicuro e affidabile. Ciò significa che puoi contare su Gleam per eseguire il parsing della risposta della pagina in modo corretto e senza dover preoccuparti di errori di tipo.

## Vedi Anche

- Documentazione ufficiale di Gleam: https://gleam.run/
- Libreria libcurl: https://curl.se/libcurl/
- Erlang typesystem: https://erlang.org/doc/reference_manual/typespec.html