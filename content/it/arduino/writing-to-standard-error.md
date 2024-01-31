---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
Scrivere su standard error (stderr) serve a separare i messaggi di errore dall'output standard (stdout). I programmatori lo fanno per diagnosticare problemi senza intaccare l'output regolare.

## Come Fare:
Arduino non ha un concetto nativo di stderr. Tuttavia, puoi simulare stderr usando `Serial.print()` per il debug.

```Arduino
void setup() {
  Serial.begin(9600); // Avvia la comunicazione seriale
}

void loop() {
  if (somethingWentWrong()) {
    Serial.print("Errore: Qualcosa è andato storto!");
  }
}
```
Output simulato di stderr:
```
Errore: Qualcosa è andato storto!
```
## Approfondimento:
In sistemi più complessi, stdout e stderr sono due canali separati. Su Arduino, entrambi sono uniti in `Serial`. In passato, su computer, stderr permetteva agli utenti di reindirizzare i messaggi d'errore. Le alternative includono il logging su SD o memória EEPROM se la separazione è vitale.

## Vedi Anche:
- Arduino Reference for `Serial`: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Understanding Streams: https://en.cppreference.com/w/cpp/io
- Debugging Techniques with Arduino: http://playground.arduino.cc/Main/GeneralCodeLibrary
