---
title:                "Gleam: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Molti linguaggi di programmazione moderni si basano su JSON (JavaScript Object Notation) come formato standard per lo scambio di dati tra diverse applicazioni. L'utilizzo di JSON può semplificare il processo di scambio di dati e rendere la programmazione più efficiente.

## Come Fare

Per lavorare con JSON in Gleam, è necessario utilizzare un modulo esterno chiamato "json". Prima di tutto, è necessario installare il modulo utilizzando il comando `rebar3 get-deps`. Una volta installato, è possibile importare il modulo nel tuo codice Gleam utilizzando `import json`.

Una delle funzioni più comuni per lavorare con JSON è `encode`, che consente di convertire una struttura di dati in un formato JSON. Ecco un esempio di come utilizzare questa funzione:

```Gleam
let data = {
    "nome": "Mario",
    "cognome": "Rossi",
    "età": 25
}

json.encode(data)
```

Questo produrrà l'output seguente:

```Gleam
"{\"nome\":\"Mario\",\"cognome\":\"Rossi\",\"età\":25}"
```

Per convertire un file JSON in una struttura dati Gleam, è possibile utilizzare la funzione `decode`, come mostrato in questo esempio:

```Gleam
let json_string = "{\"nome\":\"Mario\",\"cognome\":\"Rossi\",\"età\":25}"

json.decode(json_string)
```

Il risultato sarà una struttura dati come questa:

```Gleam
Ok({
    "nome": "Mario",
    "cognome": "Rossi",
    "età": 25
})
```

## Approfondimento

Quando si lavora con JSON in Gleam, è importante comprendere che il formato non è dinamico, il che significa che una volta che i dati sono stati convertiti in JSON, non è possibile aggiungere o rimuovere chiavi in modo dinamico. Ciò potrebbe essere un fattore importante da considerare quando si progetta una struttura dati per la conversione in JSON.

Inoltre, è possibile utilizzare i tipi di dati personalizzati di Gleam come parte della struttura di dati JSON. Ad esempio, si può creare un record con i campi "nome" e "cognome", e utilizzare quel record come valore all'interno di un altro record che rappresenta una persona. In questo modo, è possibile ottenere una struttura dati più strutturata e comprensibile quando si lavora con dati complessi.

## Vedi Anche

- [Modulo JSON di Gleam](https://github.com/gleam-lang/json)
- [JSON su Wikipedia](https://it.wikipedia.org/wiki/JavaScript_Object_Notation)
- [Guida all'utilizzo di dati JSON in Gleam](https://gleam.run/tutorials/json)