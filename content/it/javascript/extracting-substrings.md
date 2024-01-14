---
title:                "Javascript: Estrazione di sottostringhe"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore JavaScript, probabilmente hai incontrato la necessità di estrarre una sottostringa da una stringa più grande. Ci sono molti motivi per cui potresti aver bisogno di fare questo, ad esempio per manipolare le stringhe in modo più efficiente o per ottenere una parte specifica di una stringa per usarla in un altro contesto. In questo post, ti mostrerò come estrarre delle sottostringhe in JavaScript e approfondiremo le diverse opzioni a tua disposizione.

## Come
Per estrarre una sottostringa da una stringa, puoi utilizzare il metodo integrato `substring()`. Questo metodo richiede due parametri: l'indice iniziale della sottostringa e l'indice finale (opzionale). Vediamo un esempio:

```Javascript
let stringa = "Questo è un esempio di una stringa";
let sottostringa = stringa.substring(11, 18);
console.log(sottostringa); // Output: esempio
```

Se non viene specificato un secondo argomento, il metodo `substring()` estrarrà la parte della stringa a partire dall'indice iniziale fino alla fine della stringa. Puoi anche utilizzare valori negativi per gli indici, che verranno interpretati come partenza dalla fine della stringa. Vediamo un altro esempio:

```Javascript
let stringa = "Questo è un esempio di una stringa";
let sottostringa = stringa.substring(-7);
console.log(sottostringa); // Output: stringa
```

Se vuoi semplicemente estrarre una parte della stringa a partire da un indice specifico fino alla fine, puoi utilizzare il metodo `slice()`. Questo metodo funziona in modo simile a `substring()` ma è più flessibile perché ti permette di specificare solo un indice iniziale o solo un indice finale. Vediamo un esempio:

```Javascript
let stringa = "Questo è un esempio di una stringa";
let sottostringa = stringa.slice(19); // estrae la parte della stringa dopo "di"
console.log(sottostringa); // Output: una stringa
```

Entrambi i metodi `substring()` e `slice()` vengono utilizzati principalmente per estrarre sottostringhe da una stringa più grande, ma ci sono alcuni casi in cui potresti avere bisogno di estrarre caratteri specifici anziché intere sottostringhe. Per questo, JavaScript offre il metodo `charAt()`, che restituisce il carattere corrispondente all'indice specificato nella stringa. Vediamo un esempio:

```Javascript
let stringa = "Questo è un esempio di una stringa";
let carattere = stringa.charAt(8);
console.log(carattere); // Output: è
```

Oltre a questi metodi, puoi anche utilizzare le espressioni regolari per estrarre substrings da una stringa. Questo può essere utile se hai bisogno di estrarre parti della stringa che corrispondono a un determinato pattern. Vediamo un esempio con l'utilizzo dell'operatore `match()`:

```Javascript
let stringa = "Il mio numero di telefono è 3456789123";
let numero = stringa.match(/\d+/);
console.log(numero); // Output: 3456789123
```

## Deep Dive
Come puoi vedere, ci sono molte opzioni per estrarre sottostringhe in JavaScript e ognuna ha i propri casi d'uso. È importante considerare le diverse opzioni e scegliere quella più adatta alle tue esigenze. Inoltre, è importante prestare attenzione alla gestione degli indici, poiché possono causare errori se non sono gestiti correttamente.

## Vedi Anche
- [MDN: String.substring()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN: String.slice()] (https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN: String.charAt()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN: RegExp.prototype.exec()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec)