---
title:                "Arduino: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché

Se state lavorando con Arduino, probabilmente avete bisogno di manipolare o analizzare delle stringhe. Una delle operazioni più comuni che si possono fare con le stringhe è l'estrazione di sottostringhe. Ciò è utile quando si desidera ottenere solo una parte di una stringa più grande o quando si vuole analizzare i dati contenuti in una stringa.

## Come

Per estrarre una sottostringa da una stringa esistente in Arduino, è possibile utilizzare la funzione `substring()`. Questa funzione richiede tre parametri: la stringa di origine, l'indice di inizio e la lunghezza della sottostringa.

```Arduino
String stringaOriginale = "Ciao mondo!";
String sottostringa = stringaOriginale.substring(5, 6);
//sottostringa sarà uguale a "m"
```

La sottostringa viene estratta a partire dall'indice 5 (compreso) e continuerà per 6 caratteri nella stringa di origine. Si noti che gli indici in Arduino partono da zero, quindi il primo carattere ha indice 0.

È inoltre possibile utilizzare la funzione `indexOf()` per trovare l'indice di un carattere specifico all'interno della stringa e utilizzarlo per estrarre una sottostringa. Ad esempio, se si vuole estrarre solo il nome di una persona da una stringa composta da nome e cognome, si può utilizzare la funzione `indexOf()` per trovare l'indice dello spazio tra i due nomi e utilizzarlo per estrarre solo il primo nome.

## Deep Dive

Il tipo di dati `String` in Arduino include molte altre funzioni utili per manipolare le stringhe. Ad esempio, ci sono metodi per convertire le stringhe in caratteri maiuscoli o minuscoli, per sostituire una parte di una stringa con un'altra sottostringa, o per concatenare più stringhe insieme.

Inoltre, vale la pena notare che l'utilizzo di stringhe in Arduino richiede una certa quantità di memoria, quindi è consigliabile utilizzare il tipo di dati `char[]` per le stringhe più lunghe o se si lavora con un quantitativo di memoria limitato.

# Vedi anche

- [Funzioni di stringa in Arduino](https://www.arduino.cc/reference/tr/language/variables/data-types/string/functions/)
- [Guida all'utilizzo delle stringhe in Arduino](https://www.arduino.cc/en/Tutorial/String)
- [Video tutorial sull'estrazione di sottostringhe in Arduino](https://www.youtube.com/watch?v=iLQxqDtPYj4)