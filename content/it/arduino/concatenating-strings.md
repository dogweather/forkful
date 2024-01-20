---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La concatenazione di stringhe è il processo di unione di due o più stringhe in una sola. I programmatori lo fanno per combinare dati di testo in modo efficiente.

## Come Fare:

Il modo più semplice per concatenare stringhe in Arduino è usando l'operatore `+`. Ecco un esempio:

```Arduino
String nome = "Mario";
String cognome = "Rossi";
String nomeCompleto = nome + " " + cognome;
Serial.println(nomeCompleto);
```

L'output sarà `Mario Rossi`.

## Approfondimento:

La concatenazione di stringhe è un concetto fondamentale della programmazione ed è presente in molti linguaggi di programmazione da quando sono stati creati.

Un'alternativa all'operatore `+` è il metodo `concat()` di Arduino, che modifica la stringa originale anziché creare una nuova stringa:

```Arduino
String saluto = "Ciao, ";
saluto.concat("Mario");
Serial.println(saluto);
```

L'output sarà `Ciao, Mario`.

Tuttavia, tenga presente che la concatenazione di stringhe può essere una operazione ad alta intensità di memoria, soprattutto su piattaforme con risorse limitate come Arduino.

## Vedi Anche:

1. Documentazione Arduino: [String Objects](https://www.arduino.cc/reference/en/language/variables/data-types/stringobjects/)
2. Tutorial sull'uso delle Stringhe in Arduino: [Arduino String Manipulation](https://startingelectronics.org/articles/arduino/switching-arduino-on-off-string/)
3. Introduzione alla programmazione con Arduino: [Arduino Programming](https://maker.pro/arduino/tutorial/introduction-to-data-types-and-arithmetic-operators-an-arduino-tutorial)