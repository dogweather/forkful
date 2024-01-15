---
title:                "Maiuscolare una stringa"
html_title:           "Arduino: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Probabilmente ti stai chiedendo perché qualcuno vorrebbe capitalizzare una stringa usando Arduino. Beh, ci sono molte ragioni per farlo! Potresti voler visualizzare delle parole più evidenti sul tuo display LCD o semplicemente migliorare l'aspetto del tuo codice. Qualunque sia la motivazione, il processo è abbastanza semplice e oggi ti mostrerò come farlo.

## Come fare

Per capitalizzare una stringa in Arduino, useremo una funzione chiamata ```toUpperCase ()```. Questo comando trasforma tutte le lettere minuscole in maiuscole. Eccoti un esempio:

```Arduino
String s = "ciao mondo";
s.toUpperCase();
Serial.println(s);
```

Output:

```
CIAO MONDO
```

Come puoi vedere, la stringa "ciao mondo" è stata trasformata in "CIAO MONDO". Se vuoi che solo la prima lettera della tua stringa sia maiuscola, puoi utilizzare la funzione ```toUpperCase ()``` solo sulla prima posizione della stringa:

```Arduino
String s = "ciao mondo";
s[0] = toupper(s[0]);
Serial.println(s);
```

Output:

```
Ciao mondo
```

## Approfondimento

Ora che hai visto come capitalizzare una stringa, potresti chiederti come funziona esattamente la funzione ```toUpperCase ()```. Bene, quando viene eseguita, questa funzione scorre attraverso la stringa e utilizza la tabella ASCII per convertire le lettere minuscole in maiuscole. Inoltre, puoi utilizzare questa funzione con qualsiasi variabile di tipo stringa, quindi non c'è bisogno di preoccuparsi del tipo di dati che stai utilizzando.

## Vedi anche

Se vuoi approfondire ulteriormente le funzioni di manipolazione delle stringhe in Arduino, puoi guardare questi link:

- [Tutorial di Wokwi](https://wokwi.com/arduino/tutorials/string-functions)
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions)
- [Video tutorial di Jeremy Blum](https://www.youtube.com/watch?v=lWYzJleRyJA)

Prova ad utilizzare la funzione ```toUpperCase ()``` nelle tue prossime sfide di programmazione con Arduino e sperimenta con altre funzioni di manipolazione delle stringhe per migliorare le tue abilità di programmazione. Buona fortuna!