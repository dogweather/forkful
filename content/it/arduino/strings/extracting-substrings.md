---
date: 2024-01-20 17:45:08.864089-07:00
description: "How to: Estrarre sottosequenze \xE8 un'operazione comune nella programmazione\
  \ fin dai primi linguaggi, come C e Java. In Arduino, usiamo la classe `String`\u2026"
lastmod: '2024-04-05T21:53:44.433775-06:00'
model: gpt-4-1106-preview
summary: "Estrarre sottosequenze \xE8 un'operazione comune nella programmazione fin\
  \ dai primi linguaggi, come C e Java."
title: Estrazione di sottostringhe
weight: 6
---

## How to:
```Arduino
String testo = "Arduino è fantastico!";
int inizio = 9;
int fine = 19;
String sottosequenza = testo.substring(inizio, fine);

Serial.begin(9600);
Serial.println(sottosequenza);  // Stampa: "fantastico"
```

```Arduino
String frase = "Programmare con Arduino è super!";
String parola = "Arduino";
int posizione = frase.indexOf(parola);
String risultato = frase.substring(posizione, posizione + parola.length());

Serial.begin(9600);
Serial.println(risultato);  // Stampa: "Arduino"
```

## Deep Dive
Estrarre sottosequenze è un'operazione comune nella programmazione fin dai primi linguaggi, come C e Java. In Arduino, usiamo la classe `String` per rappresentare sequenze di caratteri. La funzione `substring()` è essenziale per lavorare con le stringhe in modo non distruttivo, cioè senza modificare la stringa originale.

Alternative alla `String` in Arduino includono l'uso degli array di char, che sono meno pesanti sulla memoria ma più complicati da gestire per i principianti. Estrarre substrings con array di char comporta manipolazione diretta della memoria e gestione dei puntatori.

Implementare una funzione per estrarre substrings da zero può essere un buon esercizio per capire meglio come funzionano le stringhe a basso livello.

## See Also
- Documentazione Arduino `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial per gestire stringhe con char array: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Informazioni ulteriori su puntatori e array di char: https://www.programiz.com/c-programming/c-strings
