---
title:                "Arduino: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui qualcuno potrebbe voler confrontare due date nel loro programma Arduino. Potresti voler controllare se una data è più recente di un'altra per attivare una funzione, o forse vuoi semplicemente registrare la differenza di tempo tra due eventi. Quale che sia il tuo motivo, imparare a confrontare due date nel tuo codice Arduino può essere un'abilità utile da avere.

## Come fare

Per confrontare due date nel tuo sketch Arduino, dovrai utilizzare la classe `DateTime` della libreria `Time`. Questa classe permette di creare oggetti data e utilizzarli per eseguire operazioni come il confronto. Ecco un esempio di come puoi confrontare due date:

```
#include <Time.h>

void setup() {
  // Inizializza la libreria Time
  setTime(8, 30, 0, 1, 1, 2019); // imposta la data corrente al 1 gennaio 2019 alle ore 8:30
}

void loop() {
  // Definiamo due date da confrontare
  DateTime data1(2019, 1, 1, 8, 30, 0); // 1 gennaio 2019 alle ore 8:30
  DateTime data2(2019, 1, 1, 12, 0, 0); // 1 gennaio 2019 alle ore 12:00

  // Confrontiamo le due date
  if (data1 < data2) {
    Serial.println("La data 1 è precedente alla data 2");
  } else if (data1 > data2) {
    Serial.println("La data 2 è precedente alla data 1");
  } else {
    Serial.println("Le due date sono uguali");
  }

  delay(1000); // attende un secondo prima di ripetere il confronto
}
```

L'output di questo sketch sarà il seguente:

```
La data 1 è precedente alla data 2
```

## Approfondimento

Quando si tratta di confrontare due date, ci sono alcuni dettagli che è importante tenere a mente. Ad esempio, è importante considerare il formato delle date che si stanno confrontando. Se le date sono definite con un formato diverso, il confronto potrebbe non funzionare correttamente. Inoltre, la classe `DateTime` offre anche la possibilità di confrontare solo parti specifiche delle date, come il giorno o il mese. Assicurati di esplorare tutte le opzioni offerte dalla libreria `Time` per essere sicuro di utilizzare il metodo più adatto alle tue esigenze.

## Vedi anche

- [Libreria Time](https://github.com/PaulStoffregen/Time)
- [Guida ai confronti in Arduino](https://www.arduino.cc/reference/en/language/structure/comparison-operators/)