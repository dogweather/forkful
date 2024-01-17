---
title:                "Interpolazione di una stringa"
html_title:           "C: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
L'interpolazione di una stringa è una tecnica usata dai programmatori per creare una nuova stringa combinando parti di stringhe esistenti. È utile quando si vogliono creare stringhe dinamiche o aggiungere dati variabili a una stringa.

## Come si fa:
```C
#include <stdio.h>

int main() {
  char name[20] = "John";
  int age = 24;

  printf("Ciao, mi chiamo %s e ho %d anni.", name, age);

  return 0;
}
```

Output:
```
Ciao, mi chiamo John e ho 24 anni.
```

## Approfondimento:
L'interpolazione delle stringhe ha origini nel linguaggio di programmazione Perl, ma è ora ampiamente utilizzata nei linguaggi moderni come C, Python e JavaScript. Una tecnica simile è l'interpolazione delle variabili, in cui il valore di una variabile viene inserito all'interno di una stringa invece di una parte fissa di testo.

## Vedi anche:
- [Interpolazione delle stringhe in C++](https://www.programiz.com/cpp-programming/input-output-streams#format-output)
- [Tutorial su stringhe in C](https://www.learn-c.org/en/Strings)
- [Interpolazione di stringhe in altri linguaggi](https://www.rubyguides.com/2019/02/c-string-interpolation/)