---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Articolo di Programmazione Arduino: Cancellare Caratteri Corrispondenti a un Modello

## Cosa & Perché?
Cancellare caratteri che coincidono con un modello significa eliminare specifiche sequenze di caratteri da una stringa. Questo è utile per manipolare i dati testuali, ad esempio, la pulizia dei dati o la sintassi di scripting.

## Come fare:
Ecco un esempio molto semplice che illustra come eliminare tutti i numeri da una stringa.

```Arduino
String input = "ABC123";
for (int i = 0; i < input.length(); i++) {
  if (input[i] < '0' || input[i] > '9') {
    Serial.print(input[i]);
  }
}
```
Dando come risultato:

```Arduino
"ABC"
```

## Approfondimenti
Historicamente, la necessità di eliminare caratteri corrispondenti a un modello è sorta con l'evoluzione delle espressioni regolari, un metodo per cercare pattern in una stringa. Nella programmazione Arduino, tuttavia, non c'è supporto integrato per le espressioni regolari, dunque dobbiamo trovare alternative. A parte il ciclo for esemplificato sopra, un'altra possibile soluzione può essere l'uso della funzione `replace()` di Arduino String, che sostituisce il modello desiderato con una stringa vuota.

Per quanto riguarda i dettagli di implementazione, si dovrebbe fare attenzione all'uso eccessivo della memoria, poiché Arduino ha una quantità limitata di memoria SRAM.

## Vedere Anche
Per saperne di più sulle funzioni di Arduino String, visita la pagina ufficiale della documentazione: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)