---
title:                "Arduino: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Se stai costruendo un progetto con Arduino che ha a che fare con date, come un calendario elettronico o un timer, potresti voler calcolare una data nel futuro o nel passato. Questo può essere utile per impostare gli eventi del tuo progetto in base a determinate date o per avere una visualizzazione comoda dei giorni trascorsi. In questo post, ti mostreremo come utilizzare Arduino per calcolare una data in modo facile e preciso utilizzando il linguaggio di programmazione C++.

## Come Fare

Per iniziare, devi impostare la tua scheda Arduino con il codice necessario per l'utilizzo della libreria "Time.h". Assicurati di aver scaricato e installato correttamente la libreria nel tuo ambiente di sviluppo. Una volta fatto, puoi utilizzare la funzione "dateToDays()" per convertire una data in giorni. Ad esempio, se volessi calcolare i giorni trascorsi dal 1 gennaio 1970, puoi utilizzare il seguente codice:

```
Arduino
#include <Time.h>

int giorno = 1;
int mese = 1;
int anno = 1970;

int giorni_trascorsi = dateToDays(anno, mese, giorno);

```

In questo esempio, la variabile "giorni_trascorsi" conterrà il numero totale di giorni trascorsi dal 1 gennaio 1970. Puoi poi utilizzare questa informazione per calcolare una data futura o passata. Ad esempio, per ottenere una data nel futuro, puoi aggiungere semplicemente un numero di giorni a questa variabile:

```
Arduino
int giorni_trascorsi = dateToDays(anno, mese, giorno);

// Aggiungi 10 giorni alla data
giorni_trascorsi = giorni_trascorsi + 10;

```

Per ottenere una data nel passato, puoi invece sottrarre un numero di giorni:

```
Arduino
int giorni_trascorsi = dateToDays(anno, mese, giorno);

// Sottrai 10 giorni alla data
giorni_trascorsi = giorni_trascorsi - 10;

```

Una volta ottenuto il numero di giorni desiderato, puoi utilizzare la funzione "daysToDate()" per convertirlo in una data. Ad esempio, per convertire i giorni trascorsi in una data, puoi utilizzare il seguente codice:

```
Arduino
// Converti i giorni trascorsi in una data
int converted_days = daysToDate(giorni_trascorsi);

```

Tieni presente che questa funzione restituisce i mesi, i giorni e gli anni in un formato di 3 cifre. Quindi, per ottenere una data più leggibile, puoi dividerla nei rispettivi componenti di giorno, mese e anno. Ad esempio:

```
Arduino
// Divide i componenti della data
int giorni = converted_days % 100;
int mesi = (converted_days / 100) % 100;
int anni = converted_days / 10000;

```

## Deep Dive

Calcolare una data nel futuro o nel passato è un processo piuttosto semplice una volta che hai compreso come utilizzare la libreria "Time.h" e le sue funzioni. Tuttavia, tieni presente che questa libreria utilizza il timezone GMT, quindi potresti dover effettuare alcune conversioni se hai bisogno di utilizzare i fusi orari locali.

Inoltre, puoi anche utilizzare la funzione "makeTime()" per creare una data con un orario specifico. Ad esempio, se vuoi calcolare una data e un orario nel futuro, puoi usare questa funzione per ottenere una data più precisa.

## Vedi Anche

- [Libreria Time.h di Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Documentazione Time.h](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Tutorial su Arduino e la libreria Time.h](https://www.arduino.cc/en/Tutorial/Time)