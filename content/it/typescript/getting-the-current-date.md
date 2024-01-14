---
title:    "TypeScript: Ottenera la data corrente"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché
Quando si programma in TypeScript, è importante essere in grado di gestire la data corrente. Questo può essere utile per la gestione delle date di scadenza delle attività, per il tracciamento delle modifiche o per mostrare la data di creazione di un determinato elemento. In questa guida, impareremo come ottenere la data corrente utilizzando TypeScript.

## Come fare
Per ottenere la data corrente in TypeScript, dobbiamo utilizzare la classe Date. Vediamo un esempio di codice che ci mostra come ottenere la data corrente e visualizzarla in console:

```TypeScript
const today = new Date();
console.log(today);
```

Questo codice creerà un nuovo oggetto di tipo Date, che rappresenta la data e l'ora correnti. Quindi, verrà visualizzata la data e l'ora attuali nella console.

Ma cosa succede se vogliamo visualizzare la data in un formato diverso? Possiamo utilizzare i metodi disponibili nella classe Date per ottenere le informazioni desiderate. Ad esempio, possiamo usare il metodo `getDate()` per ottenere il giorno del mese:

```TypeScript
const today = new Date();
const day = today.getDate();
console.log(day);
```

Questo codice ci restituirà il giorno del mese corrente, ad esempio se oggi è il 12, il nostro output sarà 12.

Ci sono molti altri metodi utili per ottenere informazioni specifiche sulla data come `getMonth()`, `getHours()`, `getMinutes()` e così via.

## Deep Dive
Se vogliamo ottenere informazioni precise sulla data, possiamo specificarle come argomenti nella creazione dell'oggetto Date. Ad esempio, se vogliamo ottenere la data del 25 dicembre 2021, possiamo fare così:

```TypeScript
const christmasDay = new Date(2021, 11, 25); // Mese è indicizzato da 0 a 11, quindi 11 rappresenta il mese di dicembre.
console.log(christmasDay);
```

Questo ci mostrerà la data del 25 dicembre 2021. Inoltre, possiamo specificare anche l'ora e il minuto desiderati nel seguente modo:

```TypeScript
const countdown = new Date(2021, 11, 25, 12, 0); // 12:00
console.log(countdown);
```

Possiamo anche utilizzare il metodo `getTime()` per ottenere il timestamp della data specificata. Il timestamp è il numero di millisecondi trascorsi dal 1° gennaio 1970 a mezzanotte, ora UTC. Possiamo utilizzare questo timestamp per eseguire operazioni matematiche sulla data o per confrontare date diverse.

## Vedi anche
- [Documentazione TypeScript su Date](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#dates)
- [Altri metodi della classe Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#methods_2)