---
title:                "Javascript: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Perché
Lavorare con JSON è fondamentale per creare applicazioni dinamiche e interattive. JSON (JavaScript Object Notation) è un formato di dati che permette di scambiare informazioni tra diverse piattaforme e linguaggi di programmazione. È ampiamente utilizzato per rappresentare dati strutturati, come ad esempio i dati scambiati tra il browser e un server web.

## Come
Per lavorare con JSON in Javascript, è necessario avere una buona conoscenza delle basi della programmazione e del linguaggio Javascript. Il primo passo è comprendere la sintassi di JSON, che prevede l'utilizzo di coppie chiave-valore, racchiuse tra parentesi graffe. Ad esempio:

```Javascript
{
    "nome": "Mario",
    "cognome": "Rossi",
    "eta": 35
}
```

Per accedere ai dati in un oggetto JSON, possiamo utilizzare la notazione con il punto, ad esempio per ottenere il valore del nome possiamo usare `obj.nome`.

Inoltre, per manipolare i dati e modificarli, è possibile utilizzare i metodi `JSON.stringify()` e `JSON.parse()`. Il primo converte un oggetto Javascript in una stringa JSON, mentre il secondo converte una stringa JSON in un oggetto Javascript. Ad esempio:

```Javascript
let oggetto = {
    "nome": "Mario",
    "cognome": "Rossi",
    "eta": 35
};

let stringa = JSON.stringify(oggetto); // converte l'oggetto in una stringa JSON
console.log(stringa); // output: {"nome":"Mario","cognome":"Rossi","eta":35}

let nuovoOggetto = JSON.parse(stringa); // converte la stringa in un oggetto Javascript
console.log(nuovoOggetto.nome); // output: Mario
```

## Deep Dive
Oltre alla sintassi di base, è importante comprendere alcuni concetti fondamentali quando si lavora con JSON. Ad esempio, è possibile utilizzare un array di oggetti JSON, che rappresenta una lista di elementi. In questo caso, la sintassi prevede l'utilizzo delle parentesi quadre. Ad esempio:

```Javascript
const persone = [
    {
        "nome": "Mario",
        "cognome": "Rossi"
    },
    {
        "nome": "Anna",
        "cognome": "Bianchi"
    },
    {
        "nome": "Luca",
        "cognome": "Verdi"
    }
];

console.log(persone[0].cognome); // output: Rossi
```

Inoltre, è possibile utilizzare JSON per passare dati tra il browser e un server web, ad esempio tramite una richiesta AJAX. In questo caso, la stringa JSON rappresenta il corpo della richiesta e permette di scambiare informazioni in maniera efficiente e organizzata.

## See Also
- [JSON Tutorial by W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Manipulating JSON with JavaScript](https://www.blog.duomly.com/manipulating-json-with-javascript/)
- [Handling JSON in Javascript](https://medium.com/swlh/handling-json-in-javascript-f7b9c68afa2b)