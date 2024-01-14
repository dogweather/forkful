---
title:                "Fish Shell: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Molte volte è necessario inviare una richiesta HTTP per ottenere o inviare dati da un server esterno. Questo può essere utile per l'integrazione di servizi, per eseguire una determinata operazione o ottenere informazioni aggiuntive.

## Come Fare

Per inviare una richiesta HTTP in Fish Shell, dobbiamo utilizzare il comando `curl`. Di seguito è riportato un esempio di codice che utilizza `curl` per inviare una richiesta POST con alcuni dati da un modulo:

```Fish Shell
curl -X POST -d "nome=Giulia&età=25" https://www.example.com/api/utenti
```

Il comando `curl` consente di specificare il metodo della richiesta utilizzando l'opzione `-X` e di passare i dati tramite l'opzione `-d`. Alcuni altri parametri utili sono:

- `-H` per specificare l'intestazione della richiesta 
- `-i` per includere gli header di risposta nella risposta 
- `-o` per salvare la risposta in un file anziché stamparla nel terminale 

Inoltre, possiamo utilizzare le variabili Fish Shell per automatizzare la costruzione della richiesta. Ad esempio, possiamo creare una variabile per il nostro URL di destinazione:

```Fish Shell
set url https://www.example.com/api/utenti
curl -X POST -d "nome=Giulia&età=25" $url
```

Possiamo anche gestire le risposte ottenute dalla nostra richiesta utilizzando il comando `jq` per analizzare i dati in formato JSON. Ad esempio, se volessimo ottenere l'ID dell'utente creato nella nostra richiesta POST, potremmo utilizzare il seguente codice:

```Fish Shell
set response (curl -X POST -d "nome=Giulia&età=25" $url | jq '.id')
echo $response
```

In questo modo, abbiamo salvato l'ID dell'utente creato nella variabile `$response` e poi lo abbiamo stampato nel terminale. 

## Deep Dive

Per inviare una richiesta HTTP in modo più avanzato, possiamo anche utilizzare il modulo "Fish Web" che fornisce funzioni per la creazione, l'invio e la gestione delle richieste HTTP. Possiamo installare il modulo utilizzando il seguente comando:

```Fish Shell
fisher install jorgebucaran/fish-web
```

Una volta installato, possiamo utilizzarlo nel nostro codice in questo modo:

```Fish Shell
source (fish web/setup)
set url https://www.example.com/api/utenti
set data ["nome=Giulia" "età=25"]
set result (web::request $url --method POST --form $data)
echo $result
```

Questo esempio utilizza il modulo "Fish Web" per creare una richiesta POST utilizzando l'intestazione `Content-Type: application/x-www-form-urlencoded` e passando i dati come una lista di coppie chiave-valore.

## Vedi Anche

- Documentazione Fish Shell su `curl`: https://fishshell.com/docs/current/cmds/curl.html
- Documentazione Fish Shell su `jq`: https://fishshell.com/docs/current/cmds/jq.html
- Repository GitHub di "Fish Web": https://github.com/jorgebucaran/fish-web