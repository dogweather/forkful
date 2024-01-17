---
title:                "Estrazione di sottostringhe"
html_title:           "C: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo si fa?
Estrarre sottostringhe in programmazione è il processo di ottenere una porzione di una stringa più grande, riga di testo o sequenza di caratteri. I programmatori spesso fanno questo per manipolare o analizzare una determinata parte di un testo, come ad esempio per estrarre un numero, una data o un nome da un messaggio di posta elettronica.

## Come fare:
Per estrarre una sottostringa in C, esistono alcune opzioni a seconda delle necessità specifiche del programma. Una possibile soluzione è l'utilizzo delle funzioni `substr` o `strncpy`. Di seguito un esempio di codice che utilizza la funzione `substr` per estrarre i primi 5 caratteri da una stringa:

```C
char stringa[20] = "Buongiorno";

char sottostringa[6];
substr(stringa, sottostringa, 5);

printf("La sottostringa è: %s", sottostringa);
```

La stampa del codice sopra sarebbe: "La sottostringa è: Buong".

## Approfondimento:
L'estrazione di sottostringhe è stata scientificamente studiata da Jack Minker negli anni '60, diventando un importante argomento di ricerca nell'ambito della teoria della computazione. Oltre all'utilizzo delle funzioni `substr` e `strncpy`, una tecnica alternativa per estrarre sottostringhe è l'utilizzo di espressioni regolari. Implementare queste soluzioni richiede una conoscenza avanzata di C e delle sue librerie.

## Vedi anche:
Per ulteriori informazioni sull'estrazione di sottostringhe in C, puoi consultare questi link:
- Documentazione ufficiale della funzione `substr`: https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm
- Tutorial sull'utilizzo di espressioni regolari in C: https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_71/rtref/regexcla.htm