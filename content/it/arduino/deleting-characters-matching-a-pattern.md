---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "Arduino: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Che cos'è e perché?
Cancellare i caratteri corrispondenti a un determinato modello è un processo molto utile per i programmatori. Si tratta di eliminare tutti i caratteri che seguono una specifica corrispondenza, rendendo il codice più pulito e ottimizzato.

# Come fare:
Un modo semplice per cancellare i caratteri corrispondenti a un modello è utilizzando la funzione "replace" di Arduino. Di seguito è riportato un esempio di codice che utilizza questa funzione:

```Arduino
String text = "Questo è un esempio di codice: ABC123";
text.replace("ABC", "");
Serial.println(text);
```

L'output di questo codice sarà "Questo è un esempio di codice: 123", poiché la funzione ha eliminato tutti i caratteri corrispondenti al modello "ABC".

# Approfondimenti:
La cancellazione dei caratteri corrispondenti a un modello è stata introdotta per la prima volta nel linguaggio di programmazione AWK, più di 40 anni fa. Oggi, questa funzionalità è presente in molti linguaggi di programmazione, tra cui Arduino.

Un'alternativa alla funzione "replace" di Arduino è l'utilizzo di espressioni regolari. Queste sono sequenze di caratteri che consentono di identificare e manipolare testi in modo molto preciso. Se vuoi saperne di più sulle espressioni regolari, puoi consultare [questo articolo](https://medium.com/@167/le-espressioni-regolari-dmca4e678cc6) in italiano.

Per quanto riguarda l'implementazione della funzione "replace" in Arduino, è possibile utilizzare anche altri parametri per specificare la posizione del carattere da eliminare o la quantità di caratteri da cancellare.

# Vedi anche:
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/language/variables/string/functions/replace/)
- [Tutorial su espressioni regolari in Arduino](https://www.hackster.io/Arduino_Genuino/regular-expressions-c25042)