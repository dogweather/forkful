---
title:                "Convertire una stringa in maiuscolo"
html_title:           "Java: Convertire una stringa in maiuscolo"
simple_title:         "Convertire una stringa in maiuscolo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?
La capitalizzazione di una stringa in informatica si riferisce alla pratica di rendere maiuscola la prima lettera di ogni parola in una stringa. Ciò rende più leggibile e ordinato il testo, soprattutto quando si tratta di titoli o di testi inseriti dall'utente. I programmatori spesso lo fanno per motivi di estetica e facilmente leggibilità del codice.

## Come fare:
```Java
String stringa = "questa è una stringa di esempio";

String stringaCapitalizzata = stringa.substring(0,1).toUpperCase() + stringa.substring(1);

System.out.println(stringaCapitalizzata);
```

Output: 
```
Questa è una stringa di esempio
```

## Approfondimento:
La pratica di capitalizzare una stringa è stata introdotta già negli anni '60 insieme alla creazione delle prime librerie di programmazione. Ci sono anche alternative come l'utilizzo di tag HTML o l'utilizzo di funzioni di formattazione nei linguaggi di programmazione moderni. L'implementazione della capitalizzazione di una stringa può variare a seconda del linguaggio utilizzato, ma il concetto di base rimane lo stesso.

## Vedi anche:
https://www.techopedia.com/definition/7491/capitalize
https://www.geeksforgeeks.org/capitalize-first-letter-of-each-word-in-a-string-in-java/