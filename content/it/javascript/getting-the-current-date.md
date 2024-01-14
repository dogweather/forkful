---
title:    "Javascript: Ottenere la data corrente"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Perché
Se sei un programmatore JavaScript, probabilmente hai bisogno di utilizzare la data corrente in diversi script. Ci sono molte ragioni per cui potresti avere bisogno di ottenere la data corrente, ad esempio per visualizzarla in un sito web, per effettuare calcoli di tempo o per registrare l'orario di una determinata azione.

# Come
È possibile ottenere la data corrente utilizzando l'oggetto Date integrato in JavaScript. Questo oggetto rappresenta una data e un orario specifici e offre molte opzioni per ottenere il valore della data corrente.

Esempio di codice:
```
var dataCorrente = new Date(); 
console.log(dataCorrente);
```
Questo codice creerà un nuovo oggetto Date con la data e l'ora correnti e la visualizzerà nella console del browser.

Ci sono anche altre opzioni per ottenere parti specifiche della data, come ad esempio il giorno, il mese o l'anno. Ecco un esempio di codice per ottenere l'anno corrente:
```
var anno = dataCorrente.getFullYear();
console.log(anno);
```
Questo codice utilizzerà il metodo getFullYear() per ottenere l'anno corrente dalla data e lo visualizzerà nella console.

# Approfondimento
Oltre alle funzioni di base per ottenere la data corrente, l'oggetto Date offre anche metodi per gestire le differenze di fuso orario e per eseguire operazioni matematiche sulla data, come ad esempio aggiungere o sottrarre giorni o mesi.

Ad esempio, il seguente codice aggiunge 7 giorni alla data corrente e la visualizza nella console:
```
dataCorrente.setDate(dataCorrente.getDate() + 7);
console.log(dataCorrente);
```

È importante ricordare che l'oggetto Date utilizza un sistema di numerazione dei mesi da 0 a 11, quindi il mese di gennaio viene rappresentato dal numero 0 e il mese di dicembre dal numero 11.

# Vedi anche
- Documentazione su Date su MDN: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date
- Tutorial su Date su W3Schools: https://www.w3schools.com/jsref/jsref_obj_date.asp
- Esempi di utilizzo di Date su Stack Overflow: https://stackoverflow.com/questions/tagged/javascript+date