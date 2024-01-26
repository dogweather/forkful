---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Le espressioni regolari consentono di analizzare le stringhe di testo per cercare modelli. I programmatori le utilizzano per validare l’input, estrarre dati, e semplificare la ricerca e la sostituzione di testo.

## Come fare:

```Arduino
#include <regex.h>

const char *stringaTest = "Ciao, io sono il numero 12345";
regex_t regex;
int risultato;

void setup() {
  Serial.begin(9600);
  if (regcomp(&regex, "\\b\\d{5}\\b", REG_EXTENDED) == 0) { // Cerca 5 numeri consecutivi
    Serial.println("Espressione regolare compilata correttamente.");
  } else {
    Serial.println("Errore nella compilazione dell'espressione regolare.");
  }
  risultato = regexec(&regex, stringaTest, 0, NULL, 0);
  if (!risultato) {
    Serial.println("Corrispondenza trovata!");
  } else if (risultato == REG_NOMATCH) {
    Serial.println("Nessuna corrispondenza trovata.");
  } else {
    Serial.println("Errore nell'esecuzione dell'espressione regolare.");
  }
  regfree(&regex);
}

void loop() {
  // Esempio non richiede loop.
}
```

Output:
```
Espressione regolare compilata correttamente.
Corrispondenza trovata!
```

## Approfondimento

Le espressioni regolari esistono fin dagli anni '50. In Arduino, la libreria `<regex.h>` è spesso meno potente rispetto a linguaggi come Python o JavaScript. Come alternativa, le funzioni `String` native possono essere usate per operazioni più semplici su stringhe. Per i microcontroller, l'implementazione di espressioni regolari deve essere leggera a causa delle limitate risorse hardware.

## Altre Risorse

- [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [RegexOne: Imparare le Espressioni Regolari](https://regexone.com)
- [Stack Overflow: RegEx Matching in Arduino](https://stackoverflow.com/questions/tagged/regex+arduino)
