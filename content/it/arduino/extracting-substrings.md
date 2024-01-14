---
title:    "Arduino: Estrarre sottostringhe"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Molti di voi avranno incontrato la necessità di estrarre determinate porzioni di testo da una stringa più grande. Magari vi siete trovati a dover leggere un codice a barre o un QR code e avete bisogno di estrarre solo l'ID contenuto all'interno. In questi casi, l'estrazione delle sottostringhe può essere una skill utile da avere nel vostro repertorio di programmazione con Arduino.

## Come Fare

Estrazione delle sottostringhe su Arduino è un processo abbastanza semplice. Basta seguire questi passaggi e sarete in grado di estrarre qualsiasi sottostringa di cui avete bisogno.

Per prima cosa, dobbiamo dichiarare la nostra stringa originale e una serie di variabili che ci aiuteranno nell'estrazione.

```Arduino
String frase = "Ciao a tutti!";
int posIniziale = 5; //indice della posizione iniziale della sottostringa
int lunghezza = 6; //lunghezza della sottostringa che vogliamo estrarre
String sottostringa; //variabile in cui verrà salvata la sottostringa estratta
```

Ad esempio, se vogliamo estrarre la parola "tutti" dalla nostra stringa originale, possiamo utilizzare la funzione `substring()` di Arduino in questo modo:

```Arduino
sottostringa = frase.substring(posIniziale, posIniziale + lunghezza);
```

La funzione `substring()` richiede due parametri: la posizione iniziale della sottostringa e la sua lunghezza. Se eseguite il codice, la variabile `sottostringa` conterrà il valore "tutti".

## Approfondimento

Oltre alla funzione `substring()`, Arduino ha a disposizione altre funzioni utili per l'estrazione delle sottostringhe. Ad esempio, la funzione `indexOf()` ci permette di trovare la posizione di una specifica parola o carattere all'interno della stringa originale. Possiamo poi utilizzare questo valore per estrarre una sottostringa in base alla sua posizione.

```Arduino
String frase = "Benvenuti in Arduino!";
int posizione = frase.indexOf("Arduino");
String sottostringa = frase.substring(posizione, posizione + 7);
```

In questo caso, la variabile `sottostringa` conterrà il valore "Arduino".

## Vedi Anche

Per ulteriori informazioni su come lavorare con le stringhe su Arduino, vi consiglio di consultare questi link:

- [Documentazione ufficiale di Arduino sulle stringhe](https://www.arduino.cc/reference/it/language/variables/data-types/string/)
- [Tutorial di Adafruit sull'utilizzo delle stringhe su Arduino](https://learn.adafruit.com/memories-of-an-arduino/string-basics)
- [Video tutorial di GreatScott! sulla manipolazione delle stringhe su Arduino](https://www.youtube.com/watch?v=vG3in9aY6hU)