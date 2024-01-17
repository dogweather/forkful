---
title:                "Scrittura di test"
html_title:           "Arduino: Scrittura di test"
simple_title:         "Scrittura di test"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test è il processo di creazione di codice che verifica il corretto funzionamento di altre parti del codice. I programmatori scrivono test per garantire che il loro codice sia affidabile e senza errori, consentendo loro di individuare e risolvere eventuali bug in modo più rapido ed efficiente.

## Come fare:

Per scrivere test su Arduino, è possibile utilizzare la libreria integrata "Arduino Unit Testing", che consente di eseguire test sul codice del progetto. Di seguito è riportato un esempio di codice che esegue un semplice test su una funzione:

```
ArduinoUnitTesting myTest;
int input = 5;
int expectedOutput = 10;

int actualOutput = myFunction(input);

myTest.assertEquals(expectedOutput, actualOutput);
```

Se il test fallisce, verrà visualizzato un messaggio di errore indicando quale è il valore atteso e quale è il valore effettivamente ottenuto.

## Approfondimento:

Per eseguire test su Arduino, è possibile utilizzare anche altre librerie come "Unity" e "CppUTest", che offrono maggiori funzionalità e opzioni di personalizzazione. Inoltre, alcune schede di sviluppo di Arduino, come la Arduino Mega, offrono la possibilità di eseguire test di unità hardware tramite pin specifici.

## Vedi anche:

- Arduino Unit Testing: https://www.arduino.cc/reference/en/libraries/arduinounittesting/
- Unity: https://github.com/ThrowTheSwitch/Unity
- CppUTest: https://cpputest.github.io/
- Schede di sviluppo Arduino: https://store.arduino.cc/