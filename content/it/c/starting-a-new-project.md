---
title:    "C: Iniziando un nuovo progetto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Uno dei motivi più comuni per iniziare un nuovo progetto in C è per sviluppare un'applicazione o un sistema che richiede prestazioni ottimali e una gestione efficiente delle risorse di sistema. Inoltre, C è ancora molto utilizzato nelle applicazioni di sistema, nei sistemi operativi e nelle reti.

## Come fare

Iniziamo esaminando un esempio di codice in C per stampare la frase "Ciao, mondo!" sul terminale:

```C
#include <stdio.h>

int main()
{
    printf("Ciao, mondo!");
    return 0;
}
```

Nel codice sopra, stiamo includendo la libreria standard "stdio.h" per poter utilizzare la funzione di stampa "printf". All'interno della funzione "main", utilizziamo la funzione "printf" per stampare la stringa desiderata e terminate il programma con "return 0". Il risultato del programma sarà la stampa della frase "Ciao, mondo!" sul terminale.

Ora esaminiamo un esempio più complesso che coinvolge l'utilizzo di variabili e operatori matematici per calcolare l'area di un cerchio. 

```C
#include <stdio.h>

int main()
{
    // Dichiarazione delle variabili
    double r, area;
    const double pi = 3.14159;

    // Input
    printf("Inserisci il raggio del cerchio: ");
    scanf("%lf", &r);

    // Calcolo dell'area
    area = pi * r * r;

    // Output
    printf("L'area del cerchio con raggio %lf è: %lf", r, area);
    return 0;
}
```

Nel codice sopra, dichiariamo due variabili, "r" per il raggio e "area" per il risultato del calcolo. Utilizziamo inoltre la parola chiave "const" per dichiarare una costante, in questo caso il valore di π. Utilizziamo quindi la funzione "scanf" per acquisire un valore da input dell'utente e, tramite l'operatore matematico "*", calcoliamo l'area del cerchio e la salviamo nella variabile "area". Infine, utilizziamo nuovamente la funzione "printf" per stampare il risultato sul terminale.

## Approfondimento

Prima di iniziare un nuovo progetto in C, è importante avere una conoscenza approfondita del linguaggio e delle sue caratteristiche. Inoltre, è consigliato utilizzare un editor di testo specializzato per la scrittura di codice, come ad esempio Visual Studio Code o Sublime. Inoltre, è possibile utilizzare un debugger per individuare eventuali errori nel codice e una libreria di standard per poter utilizzare funzioni predefinite.

## Vedi anche

- [Tutorial di C su Programiz](https://www.programiz.com/c-programming)
- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Esercizi e soluzioni di Programmazione in C](https://www.w3resource.com/c-programming-exercises/)