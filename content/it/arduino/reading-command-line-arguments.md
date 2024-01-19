---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e Perche?
La lettura degli argomenti da riga di comando consiste nel processare le opzioni/argomenti immessi durante l'esecuzione di un programma. I programmatori lo fanno per personalizzare il comportamento di un programma, rendendolo più flessibile e utile.

## Come si fa:
Ecco un semplice esempio di come leggere gli argomenti da riga di comando in Arduino. Ricorda, Arduino non supporta direttamente la lettura degli argomenti da riga di comando, quindi questo è soltanto un esempio ipotetico.

```Arduino
char* argomenti[] = {"programma", "arg1", "arg2", NULL};

void setup() {
  Serial.begin(9600);
  while (!Serial);
  for (int i = 0; argomenti[i]; i++) {
    Serial.println(argomenti[i]);
  }
}

void loop() {
  // qui il magico loop
}
```

Una volta caricato e avviato, dovrebbe stampare:

```
programma
arg1
arg2
```

## Approfondimento
La lettura degli argomenti da riga di comando ha avuto origine nei primi giorni dei sistemi operativi CLI (Command-Line Interface), per permettere agli utenti di personalizzare l'esecuzione del programma. Con Arduino, si potrebbe ottenere un effetto simile utilizzando l'interfaccia seriale per inserire i dati nel programma.

Una alternativa potrebbe essere l'uso di file di configurazione per memorizzare le impostazioni, oppure l'immissione dei dati tramite l'interfaccia utente del programma se disponibile.

In termini di implementazione, la lettura degli argomenti da riga di comando non è direttamente supportata da Arduino perché non ha un vero sistema operativo o una shell da riga di comando. Tuttavia, può essere emulata mediante l'uso della seriale.

## Per saperne di più
Per ulteriori informazioni o per approfondire, ecco alcuni link utili:

- [Arduino Reference](https://www.arduino.cc/reference/en/)
- [Processing Command Line Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [Command Line Arguments on Wikipedia](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)