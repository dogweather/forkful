---
title:    "Javascript: Unendo stringhe"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una pratica comune nella programmazione Javascript che consente di unire due o più stringhe in una sola. Questo può essere utile per costruire dinamicamente URL, creare messaggi personalizzati o modificare le informazioni visualizzate sulle pagine web.

## Come fare

Per concatenare le stringhe in Javascript, è possibile utilizzare l'operatore "+" o il metodo ".concat()". Ecco un esempio di entrambi i modi:

```Javascript
let nome = "Maria";
let cognome = "Rossi";
let nomeCompleto1 = nome + " " + cognome;
let nomeCompleto2 = nome.concat(" ", cognome);

console.log(nomeCompleto1); // output: Maria Rossi
console.log(nomeCompleto2); // output: Maria Rossi
```

In questo esempio, abbiamo creato due variabili con il nome e il cognome di una persona e le abbiamo concatenate in un nome completo utilizzando sia l'operatore "+" che il metodo ".concat()". È possibile notare che i due metodi raggiungono lo stesso risultato.

## Approfondimento

La concatenazione di stringhe può anche essere utilizzata per formattare e visualizzare le informazioni in modo più leggibile. Ad esempio, è possibile concatenare variabili numeriche con stringhe per creare messaggi personalizzati, come nell'esempio seguente:

```Javascript
let temperatura = 25;
let messaggio = "La temperatura attuale è di " + temperatura + " gradi celsius.";

console.log(messaggio); // output: La temperatura attuale è di 25 gradi celsius.
```

Inoltre, è possibile concatenare più stringhe in successione per costruire URL dinamicamente. Ad esempio, se si sta lavorando su un'applicazione web che richiede di costruire URL per le diverse pagine, la concatenazione di stringhe può essere utile per aggiungere i parametri necessari alle URL. Ecco un possibile esempio:

```Javascript
let baseURL = "www.example.com";
let pagina = "prodotti";
let categoria = "elettronica";
let filtro = "prezzo>1000";
let url = baseURL + "/" + pagina + "/" + categoria + "?filtro=" + filtro;

console.log(url); // output: www.example.com/prodotti/elettronica?filtro=prezzo>1000
```

## Vedi anche

- [W3Schools: Concatenazione di stringhe in Javascript](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN Web Docs: Concatenazione di stringhe in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat)