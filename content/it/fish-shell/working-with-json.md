---
title:                "Lavorare con json"
html_title:           "Fish Shell: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di programmazione e stai cercando di espandere le tue conoscenze, potresti essere interessato a lavorare con JSON. Questo formato di dati è ampiamente utilizzato nell'ambito della programmazione web e imparare a lavorare con esso può aprire molte opportunità di lavoro.

## Come Fare

Per iniziare a lavorare con JSON, prima di tutto dovrai accedere alla tua shell Fish. Questo può essere fatto facilmente digitando il comando "fish" nel tuo terminale. Una volta nell'ambiente Fish Shell, puoi utilizzare il comando "set" per creare una variabile e assegnarle dei valori in formato JSON. Ad esempio:

```Fish Shell
set myvar '{"nome": "Mario", "cognome": "Rossi", "età": 35}'
```
Una volta creata la tua variabile, puoi utilizzarla all'interno di uno script Fish per manipolare o leggere i dati in formato JSON. Ad esempio, per accedere all'età nella variabile "myvar" possiamo utilizzare il comando "echo" come segue:

```Fish Shell
echo $myvar[età]
```

Questo ci restituirà il valore "35". È anche possibile utilizzare strumenti come "jq" per formattare e filtrare i dati JSON. Ad esempio, per visualizzare solo il nome e il cognome nella nostra variabile "myvar", possiamo utilizzare il comando:

```Fish Shell
echo $myvar | jq '.nome, .cognome'
```

Questo ci restituirà il seguente output:

```
"Mario"
"Rossi"
```

## Approfondimenti

JSON è un formato di dati molto versatile e ci sono molti strumenti e librerie disponibili per lavorare con esso. Puoi esplorare ulteriormente le funzionalità di Fish Shell utilizzando la documentazione ufficiale e cercando tutorial e risorse online.

## Vedi Anche

- Documentazione ufficiale di Fish Shell (https://fishshell.com/docs/current/)
- Libreria JQ (https://stedolan.github.io/jq/)
- Guida introduttiva a JSON (https://www.json.org/json-it.html)