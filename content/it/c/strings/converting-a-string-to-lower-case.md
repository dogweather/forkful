---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:38.253375-07:00
description: "Convertire una stringa in minuscolo in C comporta la trasformazione\
  \ di tutte le lettere maiuscole presenti in una stringa data nelle loro equivalenti\u2026"
lastmod: '2024-03-13T22:44:43.891833-06:00'
model: gpt-4-0125-preview
summary: Convertire una stringa in minuscolo in C comporta la trasformazione di tutte
  le lettere maiuscole presenti in una stringa data nelle loro equivalenti minuscole.
title: Convertire una stringa in minuscolo
weight: 4
---

## Cosa e perché?

Convertire una stringa in minuscolo in C comporta la trasformazione di tutte le lettere maiuscole presenti in una stringa data nelle loro equivalenti minuscole. I programmatori spesso eseguono questa operazione per standardizzare l'input di testo per confronto, operazioni di ricerca, o semplicemente per una coerenza estetica nell'output.

## Come fare:

C non ha una funzione integrata per la conversione diretta di stringhe in minuscolo, a differenza di alcuni linguaggi di alto livello. Tuttavia, il processo può essere facilmente implementato utilizzando le funzioni della biblioteca standard di C. Di seguito è presente una guida passo dopo passo e un esempio che illustra come convertire una stringa in minuscolo.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Originale: %s\n", text);

    toLowerCase(text);
    printf("Minuscolo: %s\n", text);

    return 0;
}
```

**Esempio di output:**

```
Originale: Hello, World!
Minuscolo: hello, world!
```

In questo esempio, la funzione `toLowerCase` scorre ogni carattere della stringa di input, convertendolo nel suo equivalente minuscolo usando la funzione `tolower` da `ctype.h`. La modifica viene effettuata sul posto, alterando la stringa originale.

## Approfondimento

La funzione `tolower` utilizzata nell'esempio sopra fa parte della libreria standard di C, specificatamente all'interno del file di intestazione `ctype.h`. Opera in base alla località corrente, ma per la località standard "C", gestisce il set di caratteri ASCII dove da 'A' a 'Z' vengono convertite in 'a' a 'z'.

Storicamente, la gestione della codifica dei caratteri e della conversione in minuscolo in C era strettamente collegata al set di caratteri ASCII, limitando la sua utilità in applicazioni internazionali o localizzate dove i caratteri al di fuori del set ASCII sono comuni. I linguaggi di programmazione moderni potrebbero offrire metodi di stringa integrati per eseguire la conversione del caso considerando la località e i caratteri Unicode, cosa che C manca nativamente.

In scenari che richiedono una manipolazione del testo estensiva, specialmente con caratteri non ASCII, i programmatori potrebbero considerare l'utilizzo di librerie che offrono un migliore supporto all'internazionalizzazione, come ICU (International Components for Unicode). Tuttavia, per la maggior parte delle applicazioni che trattano testo ASCII, l'approccio dimostrato è efficiente e semplice. Questo evidenzia la propensione di C a dare ai programmatori il controllo sulla manipolazione dei dati, sebbene con un po' più di impegno rispetto ai linguaggi di livello superiore.
