---
title:                "Ricerca e sostituzione di testo"
html_title:           "Arduino: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Il processo di ricerca e sostituzione di testo è fondamentale per modificare e aggiornare facilmente il codice dei tuoi progetti Arduino. Con questo semplice meccanismo, puoi sostituire rapidamente parti del tuo codice anziché modificarle manualmente, risparmiando tempo e fatica.

## Come fare

```Arduino
String testo = "Ciao a tutti!";
testo.replace("Ciao", "Salve");
Serial.println(testo);
```

L'output di questo codice sarà "Salve a tutti!", dove la stringa "Ciao" è stata sostituita con "Salve". Puoi utilizzare il metodo `replace()` per sostituire parti di una stringa con un'altra stringa fornita come argomento.

```Arduino
String testo = "La lampadina è accesa.";
testo.replace("accesa", "spenta");
Serial.println(testo);
```

In questo caso, il risultato sarà "La lampadina è spenta.", dove la parola "accesa" è stata sostituita con "spenta". Puoi anche utilizzare questo meccanismo per rimuovere parti di una stringa, semplicemente sostituendole con una stringa vuota.

Per sostituire più occorrenze di una parola o di una frase nel tuo testo, puoi utilizzare il metodo `replaceAll()`:

```Arduino
String testo = "Voglio andare al mare ma oggi piove.";
testo.replaceAll("ma oggi piove.", "e domani farà bel tempo.");
Serial.println(testo);
```

L'output sarà "Voglio andare al mare e domani farà bel tempo.", dove la frase "ma oggi piove." è stata sostituita con "e domani farà bel tempo.".

## Approfondiamo

È importante notare che i metodi `replace()` e `replaceAll()` non modificano la stringa originale, ma ne restituiscono una nuova. Puoi quindi assegnare il risultato della sostituzione a una variabile per utilizzarla successivamente nel tuo codice, senza influire sulla stringa originale.

Inoltre, i metodi `replace()` e `replaceAll()` sono sensibili alle maiuscole e minuscole. Ciò significa che "ciao" e "Ciao" sono considerati due parole diverse e non verranno sostituite reciprocamente.

Un'altra opzione utile è il metodo `replaceFirst()`, che sostituisce solo la prima occorrenza della stringa fornita come argomento:

```Arduino
String testo = "Ciao a tutti! Ciao a voi!";
testo.replaceFirst("Ciao", "Salve");
Serial.println(testo);
```

L'output sarà "Salve a tutti! Ciao a voi!", dove solo la prima occorrenza della parola "Ciao" è stata sostituita con "Salve".

## Vedi anche

- Documentazione ufficiale di Arduino su `replace()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/
- Documentazione ufficiale di Arduino su `replaceAll()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replaceall/
- Documentazione ufficiale di Arduino su `replaceFirst()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replacefirst/