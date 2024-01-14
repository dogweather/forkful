---
title:    "Javascript: Ricerca e sostituzione del testo."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché
Un compito comune nella programmazione è la ricerca e sostituzione di testo. Questo è utile quando si desidera cambiare frasi o parole all'interno di un file o di una variabile. Continua a leggere per scoprire come farlo utilizzando Javascript!

## Come fare
Per eseguire la ricerca e sostituzione di testo in Javascript, è necessario utilizzare il metodo `replace()`. Questo metodo accetta due parametri: il primo è la stringa di ricerca e il secondo è la stringa di sostituzione. Ad esempio:

```Javascript
let testo = "Questo è un esempio di testo.";
let nuovoTesto = testo.replace("esempio", "esempio interessante");
console.log(nuovoTesto); // Output: "Questo è un esempio interessante di testo."
```

Come puoi vedere dall'esempio, il metodo `replace()` ha sostituito la parola "esempio" con "esempio interessante". Ma cosa succede se vuoi sostituire tutte le occorrenze della parola? Per farlo, dovrai utilizzare il cosiddetto flag "g" (globale). Ad esempio:

```Javascript
let testo = "Questo è un esempio di testo con più esempi.";
let nuovoTesto = testo.replace(/esempio/g, "esempio interessante");
console.log(nuovoTesto); // Output: "Questo è un esempio interessante di testo con più esempi interessanti."
```

Come puoi vedere, utilizzando il flag "g", tutte le occorrenze della parola "esempio" sono state sostituite con "esempio interessante".

## Approfondimento
Oltre alla sostituzione di parole specifiche, è possibile utilizzare il metodo `replace()` per eseguire ricerche e sostituzioni più complesse utilizzando le cosiddette espressioni regolari. Queste sono stringhe di caratteri che ti permettono di cercare pattern specifici e di sostituirli con il testo desiderato. Ad esempio:

```Javascript
let testo = "Numero di telefono: 1234567890.";
let nuovoTesto = testo.replace(/[0-9]/g, "*");
console.log(nuovoTesto); // Output: "Numero di telefono: **********."
```

In questo esempio, abbiamo utilizzato l'espressione regolare `[0-9]` per trovare tutti i numeri nel testo e sostituirli con l'asterisco "*". Ciò può essere utile per nascondere informazioni sensibili all'interno di un testo.

## Vedi anche
- [Metodo replace() - MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Espressioni regolari in Javascript - MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Espressioni_Regolari)
- [Tutorial su sostituzioni di testo in Javascript - DigitalOcean](https://www.digitalocean.com/community/tutorials/js-replacing-text)