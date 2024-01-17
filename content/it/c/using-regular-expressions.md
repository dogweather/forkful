---
title:                "Utilizzando le espressioni regolari"
html_title:           "C: Utilizzando le espressioni regolari"
simple_title:         "Utilizzando le espressioni regolari"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

Ciao a tutti programmatori! Oggi parliamo di un argomento fondamentale per chiunque lavori con linguaggi di programmazione: le espressioni regolari. Vediamo insieme cos'è e perché le usiamo.

## Cos'è e perché le usiamo?
Le espressioni regolari, anche conosciute come regex, sono uno strumento potente per cercare e manipolare testi all'interno di un programma. Ci permettono di cercare pattern specifici all'interno di una stringa e di eseguire operazioni su di essi. I programmatori le usano spesso per validare input utente, filtrare dati e sostituire parti di una stringa con altre.

## Come utilizzarle:
Per utilizzare le espressioni regolari in C, abbiamo bisogno di includere la libreria <regex.h>. Possiamo poi dichiarare una variabile del tipo ```regex_t``` per compilare la nostra espressione e una variabile del tipo ```regmatch_t``` per contenere i risultati. Di seguito un esempio di come cercare una stringa contenente "ciao" all'interno di un'altra stringa e sostituirla con "salve":

```
regex_t regex;
regmatch_t match;
char stringa[] = "ciao mondo!";
char espressione[] = "ciao";

regcomp(&regex, espressione, 0);
regexec(&regex, stringa, 1, &match, 0);
regsub(stringa, espressione, "salve", -1);

printf("%s", stringa);
```

L'output di questo codice sarebbe "salve mondo!". Vediamo brevemente cosa stiamo facendo qui:
- Con ```regcomp()``` stiamo compilando la nostra espressione regolare e salvandola nella variabile ```regex```.
- Con ```regexec()``` stiamo cercando l'espressione all'interno della stringa e salvando i risultati nella variabile ```match```.
- Infine, con ```regsub()``` stiamo sostituendo tutte le occorrenze dell'espressione con la stringa "salve".

## Approfondimento:
Le espressioni regolari sono nate negli anni '50 e sono state utilizzate principalmente nella famosa utility di UNIX "grep". Oltre ad essere ampiamente usate in C, troviamo supporto per le regex in molti altri linguaggi di programmazione come Python e JavaScript. C'è anche un'alternativa più moderna chiamata PCRE (Perl Compatible Regular Expressions), che offre funzionalità aggiuntive rispetto alle espressioni regolari standard di C.

## Vedi anche:
- [Guida completa alle espressioni regolari in C](https://www.tutorialspoint.com/cprogramming/c_regular_expressions.htm)
- [Documentazione ufficiale della libreria <regex.h>](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Introduzione alle espressioni regolari su Wikipedia](https://it.wikipedia.org/wiki/Espressione_regolare)