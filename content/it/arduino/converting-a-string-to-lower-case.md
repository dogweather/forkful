---
title:                "Conversione di una stringa in minuscolo"
html_title:           "Arduino: Conversione di una stringa in minuscolo"
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una stringa in lettere minuscole è un'operazione comune nella programmazione, che consente di uniformare l'input dell'utente e semplificare la manipolazione dei dati. In pratica, significa rendere tutte le lettere della stringa in minuscolo, indipendentemente dal caso in cui sono state inserite.

## Come fare:
Per convertire una stringa in lettere minuscole su Arduino, è possibile utilizzare la funzione "toLowerCase()". Di seguito è riportato un esempio di codice che mostra come utilizzarla:

```
String str = "Hello World!";
String lower_str = str.toLowerCase();
Serial.println(lower_str);
```

Questo codice restituirà "hello world!" come output sulla porta seriale, indipendentemente dal caso in cui è stata inserita la stringa originale.

## Approfondimento:
Nella programmazione, l'utilizzo di lettere minuscole è importante per garantire la correttezza dei dati e facilitare la loro elaborazione. In passato, quando le risorse di memoria erano limitate, i programmatori dovevano utilizzare metodi più complessi per convertire le stringhe in minuscolo. Oggi, con l'avvento di linguaggi di programmazione più moderni, come C++ (il linguaggio utilizzato su Arduino), la conversione è diventata più semplice grazie all'uso di funzioni predefinite come "toLowerCase()".

## Vedi anche:
Per ulteriori informazioni sulla funzione "toLowerCase()" e sull'utilizzo di stringhe su Arduino, puoi consultare la documentazione ufficiale di Arduino all'indirizzo https://www.arduino.cc/reference/en/language/variables/data-types/string/.