---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali significa creare una sequenza di numeri la cui apparenza è senza un apparente ordine. I programmatori lo fanno per rendere i risultati imprevedibili, ad esempio nei giochi o nelle simulazioni.

## Come fare:

Ecco un semplice esempio di generazione di numeri casuali in Arduino:

``` Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println(random(0,100));
  delay(1000);
}
```

Il codice sopra invia un numero casuale tra 0 e 100 al computer ogni secondo.

## Approfondimenti

- Contesto storico: Il desiderio di generare numeri casuali non è nulla di nuovo. Fin dai tempi antichi, le persone hanno usato metodi come i dadi per generare numeri casuali. L'implementazione in Arduino usa un metodo chiamato "linear congruential generator".
- Alternative: Potresti anche utilizzare un secondo pin non utilizzato per generare il rumore necessario per una sequenza di numeri veramente casuali.
- Dettagli di implementazione: La funzione `random()` in Arduino non genera veri numeri casuali. Genera piuttosto una sequenza di numeri che sembra casuale. Per la maggior parte degli usi, questa "pseudo-casualità" è sufficiente.

## Vedi anche

Link a risorse correlate per ulteriori dettagli su argomenti correlati:

- [Documentazione Arduino sulla funzione `random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Approfondimenti sui numeri pseudo-casuali](https://it.wikipedia.org/wiki/Generatore_di_numeri_pseudo-casuali)
- [Generare veri numeri casuali in Arduino](https://learn.adafruit.com/true-random-number-generator-for-arduino/overview)