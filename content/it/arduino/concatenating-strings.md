---
title:    "Arduino: Unire stringhe"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Il concatenamento di stringhe è un'operazione fondamentale per ogni programmatore Arduino. Consiste nel collegare diverse sequenze di caratteri per creare una stringa più lunga e significativa. Questa tecnica è molto utile quando si vuole formattare un messaggio o quando si vogliono unire vari pezzi di dati.

## Come Fare

Per concatenare le stringhe in Arduino, è necessario utilizzare la funzione `concat()` della libreria `String`. Ecco un esempio di codice che unisce una stringa predefinita ad una variabile:

```
String saluto = "Ciao ";
String nome = "Mario";
String messaggio = saluto.concat(nome);
Serial.println(messaggio);
```

L'output di questo codice sarà "Ciao Mario". È anche possibile concatenare più stringhe utilizzando la funzione `concat()` più volte:

```
String testo = "Questo è ";
String una = " un";
String esempio = " esempio ";
String concatenato = testo.concat(una).concat(esempio);
Serial.println(concatenato);
```

L'output del secondo codice sarà "Questo è un esempio".

Un altro modo per concatenare le stringhe è utilizzare l'operatore di somma `+`. Ad esempio:

```
String carattere1 = "A";
String carattere2 = "B";
String carattere3 = carattere1 + carattere2;
Serial.println(carattere3);
```

L'output di questo codice sarà "AB".

## Approfondimento

È importante ricordare che, a differenza di altri linguaggi di programmazione dove le stringhe sono considerate tipi di dati primitivi, in Arduino le stringhe sono oggetti. Questo significa che possono essere utilizzate anche con altre funzioni utili, come `substring()` per estrarre parti di una stringa o `startsWith()` per verificare se una stringa inizia con un determinato carattere.

Inoltre, è importante tenere conto delle prestazioni quando si utilizzano le stringhe in modo intensivo, poiché possono introdurre ritardi nell'esecuzione del programma. In questi casi, è preferibile utilizzare array di caratteri (`char`) anziché oggetti `String`.

## Vedi Anche

- [Documentazione ufficiale del metodo `concat()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Esempi di utilizzo della funzione `substring()`](https://howtomechatronics.com/tutorials/arduino/arduino-string-substring-concatenation-email-sender/)
- [Tutorial su come gestire le stringhe in modo efficiente su Arduino](https://arduino.land/FAQ/Working%20with%20strings%20efficiently/)