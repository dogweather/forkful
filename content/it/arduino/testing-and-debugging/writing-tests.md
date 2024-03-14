---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:32.962186-07:00
description: "Scrivere test nell'ambiente Arduino si riferisce al processo di creazione\
  \ di test automatizzati che validano la funzionalit\xE0 del tuo codice su dispositivi\u2026"
lastmod: '2024-03-13T22:44:43.686056-06:00'
model: gpt-4-0125-preview
summary: "Scrivere test nell'ambiente Arduino si riferisce al processo di creazione\
  \ di test automatizzati che validano la funzionalit\xE0 del tuo codice su dispositivi\u2026"
title: Scrivere test
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test nell'ambiente Arduino si riferisce al processo di creazione di test automatizzati che validano la funzionalità del tuo codice su dispositivi Arduino. I programmatori lo fanno per assicurarsi che il loro codice funzioni come previsto, ridurre i bug e migliorare la qualità dei loro progetti, aspetto particolarmente cruciale nei sistemi embedded dove il debug può essere più impegnativo.

## Come fare:

Arduino non dispone di un framework di testing integrato come alcuni altri ambienti di programmazione. Tuttavia, è possibile utilizzare librerie di terze parti come `AUnit` per il testing unitario del codice Arduino. AUnit è ispirato dalla libreria integrata di Arduino, `ArduinoUnit`, e dal framework di testing di Google, `Google Test`.

### Esempio con AUnit:

Prima, installa AUnit tramite il Gestore delle Librerie nell'IDE Arduino: vai su Sketch > Include Library > Manage Libraries... > cerca AUnit e installalo.

Quindi, puoi scrivere test in questo modo:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int pinLed = 13;
  pinMode(pinLed, OUTPUT);
  digitalWrite(pinLed, HIGH);
  assertTrue(digitalRead(pinLed));
}

test(ledPinLow) {
  const int pinLed = 13;
  pinMode(pinLed, OUTPUT);
  digitalWrite(pinLed, LOW);
  assertFalse(digitalRead(pinLed));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Vuoto
}
```
Dopo aver caricato questo test sulla tua scheda Arduino, apri il Monitor Seriale per visualizzare i risultati del test. Dovresti vedere un output che indica se ogni test è passato o fallito:

```
TestRunner iniziato su 2 test(s).
Test ledPinHigh superato.
Test ledPinLow superato.
Durata di TestRunner: 0.002 secondi.
Riassunto di TestRunner: 2 superati, 0 falliti, 0 saltati, 0 esauriti, su un totale di 2 test(s).
```

Questo semplice esempio dimostra l'uso di AUnit per testare lo stato di un pin LED. Creando test, confermi che il tuo Arduino si comporti come previsto in diverse condizioni. Con AUnit, puoi scrivere test più complessi, suite di test ed usufruire di funzionalità come timeout dei test e procedure di setup/teardown per scenari più avanzati.
