---
title:    "Arduino: Estrazione di sottostringhe"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché:

Se stai lavorando con una stringa di testo su Arduino, potresti aver bisogno di estrarre una parte specifica di quella stringa. Ad esempio, potresti voler ottenere i primi 3 caratteri o gli ultimi 5 caratteri. L'estrazione di sottostringhe è un'operazione comune nella programmazione e può semplificare il tuo codice.

## Come fare:

Per estrarre una sottostringa su Arduino, puoi utilizzare la funzione `substring()`. Questa funzione richiede due parametri: l'indice di inizio e la lunghezza della sottostringa desiderata. Ad esempio, se abbiamo una stringa `nome = "Arduino"`, possiamo estrarre la sottostringa "duin" utilizzando `nome.substring (2,4)`. Nota che l'indice inizia da 0 e il parametro della lunghezza è opzionale, quindi se ometti il secondo parametro, verrà estratta la sottostringa fino alla fine della stringa originale.

```
ArduinoString nome = "Arduino";
ArduinoString sottos = nome.substring (2,4);
Serial.println (sottos);
```

La console di Arduino visualizzerà "duin" come output.

## Approfondimento:

Esistono altre funzioni disponibili su Arduino per estrarre sottostringhe, come `startsWith()` e `endsWith()`, che verificano rispettivamente se una stringa inizia o finisce con una determinata sottostringa. Inoltre, puoi anche utilizzare la funzione `indexOf()` per trovare l'indice di inizio di una sottostringa all'interno di una stringa più grande e quindi utilizzare `substring()` per estrarla.

## Vedi anche:

- Tutorial di Arduino su le stringhe: https://www.arduino.cc/en/Tutorial/String
- Documentazione ufficiale sulle funzioni stringa di Arduino: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/substring/
- Esempi avanzati di estrazione di sottostringhe con Arduino: https://www.dummies.com/computers/arduino/how-to-extract-substrings-from-an-arduino-programming-string/