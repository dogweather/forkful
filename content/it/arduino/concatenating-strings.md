---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:06.327422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
La concatenazione di stringhe è il processo di unione di due o più stringhe di testo in una sola. I programmatori la usano per creare messaggi dinamici, componendo testi complessi da parti semplici.

## Come fare:
```Arduino
String saluto = "Ciao";
String nome = "Arduino";
String fraseCompleta = saluto + ", " + nome + "!";

Serial.begin(9600);
Serial.println(fraseCompleta); // Stampa: Ciao, Arduino!
```
Sample output:
```
Ciao, Arduino!
```

## Approfondimento
La concatenazione di stringhe è un'operazione fondamentale in programmazione, presente sin dagli albori dei linguaggi di programmazione. In Arduino, l'uso del tipo `String` per rappresentare e concatenare testi è comodo ma va usato con attenzione, soprattutto per la gestione della memoria in un ambiente con risorse limitate. Alternativamente, per ridurre il consumo di memoria si possono utilizzare i caratteri array (`char[]`) e le funzioni come `strcat` e `strcpy`, ma questi metodi sono più complessi. La classe `String` di Arduino nasconde la complessità ma può portare a frammentazione della memoria se non si gestiscono con cura reallocazioni e cancellazioni.

## Vedi anche:
- La documentazione ufficiale di Arduino sulle [Stringhe](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Tutorial sulla [concatenazione di stringhe](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
