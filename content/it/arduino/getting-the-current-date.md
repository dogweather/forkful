---
title:                "Arduino: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Perché

Se stai lavorando su un progetto che richiede di utilizzare la data corrente, sapere come ottenere la data corrente sul tuo programma Arduino è essenziale. Ciò ti consente di utilizzare la data come riferimento per diverse funzioni, come accendere una luce solo durante determinati giorni o inviare una notifica ogni volta che la data raggiunge una certa soglia.

## Come Fare

Per ottenere la data corrente sul tuo programma Arduino, puoi utilizzare la libreria "Time". Assicurati di avere la versione più recente di Arduino installata e di aver importato la libreria "Time" nel tuo progetto. Quindi, puoi utilizzare il seguente codice:

```arduino
#include <Time.h>

void setup() {
  // Inizializza la comunicazione seriale con il tuo computer
  Serial.begin(9600);

  // Imposta il time server da cui ottenere la data
  // Puoi utilizzare anche l'indirizzo IP del server se non hai un nome host
  // Puoi trovare una lista di time server disponibili su internet
  // Esempio: time.nist.gov
  setTimeServer("pool.ntp.org");

  // Inizializza la connessione al server
  setSyncProvider(getNtpTime);
}

void loop() {
  // Ottieni la data e l'ora correnti
  // Puoi utilizzare questa funzione in qualsiasi momento nel tuo programma
  time_t t = now();

  // Converte la data in una stringa leggibile
  // Puoi personalizzare il formato a tuo piacimento
  // Esempio: "dd/mm/yyyy hh:mm:ss"
  String dateString = String(day(t)) + "/" + String(month(t)) + "/" + String(year(t));

  // Stampa la data sulla seriale
  Serial.println(dateString);

  // Metti il programma in pausa per un secondo
  delay(1000);
}
```

L'output di questo codice mostrerà la data corrente in formato "gg/mm/aaaa" una volta al secondo sulla seriale.

## Deep Dive

Quando si utilizza la libreria "Time", è importante notare che il time server a cui ci si connette dovrebbe essere sempre online e fornire il tempo in UTC (Coordinated Universal Time). Inoltre, la libreria tiene conto del tempo trascorso dall'ultima sincronizzazione con il server. Ciò significa che se si stacca l'Arduino dalla corrente per un lungo periodo di tempo, la data e l'ora memorizzate saranno errate fino alla successiva sincronizzazione con il server. Per evitare questo problema, si può utilizzare un modulo RTC (Real-Time Clock) che tiene traccia del tempo in modo indipendente dall'alimentazione dell'Arduino.

Inoltre, il codice mostrato sopra utilizza solo la data e non tiene conto dell'ora. Se si desidera ottenere anche l'ora corrente, si può utilizzare la funzione "hour(t)" per estrarre l'ora dalla variabile "t".

# Vedi Anche

- [Documentazione Time.h](https://www.arduino.cc/reference/it/libraries/time/)
- [Lista di time server disponibili](https://tf.nist.gov/tf-cgi/servers.cgi)