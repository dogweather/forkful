---
title:                "Ottenere la data corrente"
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Perché dovresti preoccuparti di ottenere la data corrente? Semplice, perché è utile per molte cose come la gestione del tempo, il monitoraggio delle attività quotidiane o per creare progetti che richiedono una visualizzazione della data attuale.

## Come fare

Per ottenere la data corrente sul tuo Arduino, puoi utilizzare la funzione `now()` della libreria `Time`. Ecco un semplice esempio di codice che mostra la data attuale sul monitor seriale:

```Arduino
#include <Time.h> 

void setup() {
  Serial.begin(9600); // Inizializza la comunicazione seriale
}

void loop() {
  // Ottieni la data corrente con la funzione now()
  // La sintassi è ora() + il formato della data desiderato
  String data = String(ora()) + "/" + String(mese()) + "/" + String(anno());

  // Stampa la data sul monitor seriale
  Serial.println(data);

  // Aspetta 1 secondo prima di ripetere il loop
  delay(1000);
}
```

La console seriale dovrebbe mostrare qualcosa del tipo `dd/mm/yyyy`, dove `dd` è il giorno, `mm` il mese e `yyyy` l'anno.

Puoi anche utilizzare la funzione `dayStr()` e `monthStr()` per ottenere il giorno della settimana e il nome del mese. Ad esempio, se vogliamo mostrare la data in formato "giorno, dd mm yyyy", possiamo utilizzare questo codice:

```Arduino
#include <Time.h> 

void setup() {
  Serial.begin(9600); // Inizializza la comunicazione seriale
}

void loop() {
  // Ottieni la data corrente
  String day = dayStr(weekday()); // Ottieni il nome del giorno
  String data = day + ", " + String(ora()) + " " + monthStr(mese()) + " " + String(anno());

  // Stampa la data sul monitor seriale
  Serial.println(data);

  // Aspetta 1 secondo prima di ripetere il loop
  delay(1000);
}
```

Ecco un esempio di output: "Lunedì, 14 Settembre 2021".

## Approfondimento

Per utilizzare la libreria `Time`, devi prima impostare l'orologio interno del tuo Arduino. Per farlo, puoi utilizzare un modulo RTC (Real Time Clock) come il DS1307 o il DS3231, che ha un oscillatore di precisione e una batteria per mantenere l'orario anche quando l'Arduino è scollegato.

Puoi anche utilizzare la funzione `setTime()` per impostare manualmente l'orario all'inizio del programma. Ad esempio, se vuoi impostare l'orario alle 12:00 del 1° Gennaio 2021, puoi utilizzare questo codice:

```Arduino
setTime(12, 0, 0, 1, 1, 2021);
```

Per ulteriori informazioni sulla libreria `Time` e tutte le sue funzioni, consulta la [documentazione ufficiale](https://www.arduino.cc/en/Reference/Time).

## Vedi anche

- [Libreria Time](https://www.arduino.cc/en/Reference/Time)
- [Tutorial Clock: uso della libreria Time](https://www.arduino.cc/en/Tutorial/Clock)