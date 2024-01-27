---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa convertire ogni lettera minuscola in maiuscola. Si fa per uniformità, per esempio, nei titoli o quando si confrontano stringhe senza considerare il maiuscolo o minuscolo.

## How to:
Utilizza la funzione `toUpperCase()` per capitalizzare una stringa in Arduino. Ecco un esempio semplice:

```arduino
void setup() {
  // Avvio della comunicazione seriale
  Serial.begin(9600);
  
  // La tua stringa
  String miaStringa = "Ciao mondo!";
  
  // Capitalizzazione
  miaStringa.toUpperCase();
  
  // Stampa il risultato
  Serial.println(miaStringa);
}

void loop() {
  // Qui non mettiamo nulla per ora
}
```

Output:
```
CIAO MONDO!
```

## Deep Dive
La funzione `toUpperCase()` è stata introdotta per semplificare la manipolazione di stringhe. Prima, si doveva iterare ogni carattere convertendolo manualmente.

Le alternative includono l'uso di array di char e manipolare i codici ASCII, o l'uso di librerie esterne che offrono più funzionalità.

L'implementazione interna di `toUpperCase()` lavora direttamente sui buffer dei caratteri della stringa, evitando la creazione di nuove stringhe e risparmiando memoria.

## See Also
- Documentazione di Arduino su String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial sulla gestione delle stringhe in Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringExamples
