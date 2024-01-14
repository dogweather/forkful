---
title:                "Arduino: Lettura degli argomenti della linea di comando"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché 
Scrivere codice in un linguaggio di programmazione può sembrare intimidatorio, ma se stai lavorando con Arduino, non c'è niente di cui preoccuparsi! Imparare a leggere gli argomenti della riga di comando è fondamentale per poter creare codice più avanzato per controllare i tuoi dispositivi.

## Come Fare
Per prima cosa, assicurati di avere accesso alla console di Arduino sul tuo computer. Quindi, puoi iniziare a scrivere codice per leggere gli argomenti della riga di comando utilizzando la seguente sintassi: 

```Arduino
int argc = 0; 
char* argv[] = null; 
```

Questo ti consente di dichiarare una variabile per contare il numero di argomenti passati e un'array di caratteri per memorizzarli. 

Per esempio, se vuoi passare due argomenti nella tua riga di comando, puoi farlo scrivendo: 

```Arduino
./programma_arduino arg1 arg2
```

Se vuoi ottenere questi due argomenti nel tuo codice, puoi farlo usando il seguente codice: 

```Arduino
argc = 2; 
argv[0] = "arg1"; 
argv[1] = "arg2"; 
```

In questo modo, puoi accedere ai tuoi argomenti e utilizzarli nel tuo codice per controllare i tuoi dispositivi.

## Deep Dive 
Ora che hai imparato come leggere gli argomenti della riga di comando, potresti chiederti come puoi utilizzarli in modo più avanzato. Una delle possibilità è quella di utilizzare questi argomenti per personalizzare le impostazioni del tuo codice in base alle preferenze dell'utente. Ad esempio, se stai creando un dispositivo che può cambiare colore, puoi utilizzare gli argomenti della riga di comando per permettere all'utente di selezionare il colore desiderato.

Un'altra opzione è quella di utilizzare gli argomenti per passare informazioni cruciali al tuo codice, come un indirizzo IP o una porta per la connessione a una rete. In questo modo, puoi creare un codice più dinamico e modulare.

## Vedi Anche
- [Documentazione di Arduino](https://www.arduino.cc/reference/en/language/variables/environment-variables/argc/)
- [Guasti di Arduino: lettura degli argomenti dalla riga di comando](https://www.learncpp.com/cpp-tutorial/arguments-to-main/) 
- [Come utilizzare gli argomenti della riga di comando con Arduino su Raspberry Pi](https://www.tomshardware.com/how-to/use-arduino-command-line-interface)