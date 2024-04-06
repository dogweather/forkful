---
date: 2024-01-20 17:34:06.327422-07:00
description: 'Come fare: Sample output.'
lastmod: '2024-04-05T21:53:44.436383-06:00'
model: gpt-4-1106-preview
summary: ''
title: Concatenazione di stringhe
weight: 3
---

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
