---
title:                "Lavorare con json"
html_title:           "Gleam: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo semplice e potente per gestire i dati in formato JSON nei tuoi progetti Gleam, sei nel posto giusto! Nel nostro articolo di oggi, ti mostrerò come puoi utilizzare la libreria di Gleam per manipolare facilmente i dati JSON. Che tu sia un principiante o un esperto di programmazione, imparerai sicuramente qualcosa di nuovo e utile.

## Come fare

Prima di iniziare, assicurati di avere la libreria di Gleam installata sul tuo sistema. Puoi farlo eseguendo il seguente comando nella directory del tuo progetto:

```Gleam pkg install gleam/json```

Una volta installata la libreria, puoi iniziare ad utilizzarla nel tuo codice. Ad esempio, ecco come puoi creare un oggetto JSON vuoto:

```Gleam let json = Json.Encode.object([])```

Ora che abbiamo il nostro oggetto JSON, possiamo aggiungere dei dati al suo interno. Ad esempio, possiamo aggiungere una chiave "nome" con il valore "Giovanni" in questo modo:

```Gleam let json = Json.Encode.object([("nome", Json.Encode.string("Giovanni"))])```

Per ottenere il valore della nostra chiave "nome", possiamo fare come segue:

```Gleam let name = Json.Decode.field("nome", json) |> Json.Decode.string```

Puoi anche convertire facilmente una stringa in JSON usando la funzione `Json.Decode.string`:

```Gleam let json = Json.Decode.string(`{"nome":"Giovanni"}`)```

Una volta che hai compreso i fondamenti di come lavorare con i dati JSON in Gleam, puoi esplorare ulteriormente la libreria e tutte le sue funzionalità.

## Deep Dive

Se vuoi approfondire ancora di più il tuo apprendimento sulle manipolazioni dei dati JSON in Gleam, ecco alcune risorse utili che potresti consultare:

- La documentazione ufficiale della libreria JSON di Gleam: https://gleam.run/packages/gleam/json/latest/
- La guida per principianti su come utilizzare la libreria JSON di Gleam: https://gleam.run/articles/beginners-guide-to-the-json-library/
- Il repository GitHub della libreria JSON di Gleam, dove puoi esplorare il codice sorgente e contribuire a migliorarla: https://github.com/gleam-lang/json

## Vedi anche

- Come leggere e scrivere file JSON in Gleam: https://gleam.run/articles/how-to-read-and-write-json-files-in-gleam/ 
- Utilizzare la libreria JSON di Gleam con altri framework e librerie, come Phoenix e Postgres: https://gleam.run/articles/how-to-use-the-json-library-with-other-frameworks-and-libraries/