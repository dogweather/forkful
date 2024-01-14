---
title:    "Javascript: Confrontare due date"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Confrontare due date è un'operazione molto comune nella programmazione. Può essere utile per determinare l'ordine cronologico di eventi o per verificare se una determinata data è più recente di un'altra.

## Come fare

Per confrontare due date in Javascript, è necessario prima convertirle in oggetti Date usando il costruttore `new Date()`. Quindi è possibile utilizzare l'operatore di confronto `>` per determinare quale data è più grande. Ecco un esempio di codice che confronta due date e stampa il risultato:

```Javascript
let data1 = new Date(2021, 3, 23); // 23 aprile 2021
let data2 = new Date(2021, 2, 15); // 15 marzo 2021

if (data1 > data2) {
    console.log("La prima data è più grande della seconda");
} else {
    console.log("La seconda data è più grande della prima");
}

// Output: "La prima data è più grande della seconda"
```

In questo esempio, la prima data è più grande della seconda perché si trova in una posizione successiva nel calendario.

## Approfondimento

Nella comparazione di date in Javascript, ci sono alcune cose da tenere in considerazione:

- Le date devono essere convertite in oggetti Date altrimenti il confronto potrebbe non funzionare correttamente.
- Se si confrontano date con diverse precisioni (ad esempio una data con anno, mese e giorno e un'altra solo con anno e mese), l'operatore di confronto potrebbe non essere accurato.
- È importante prestare attenzione all'ordine delle date nel confronto. In Javascript, la prima data è considerata più grande della seconda se si trova in una posizione successiva nel calendario.

## Vedi anche

- [Documentazione di Date su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Confrontare date in Javascript](https://www.w3schools.com/js/js_date_methods.asp)