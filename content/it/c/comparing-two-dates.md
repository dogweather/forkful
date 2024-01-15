---
title:                "Confrontare due date"
html_title:           "C: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti dover confrontare due date nel tuo codice in C. Ad esempio, potresti voler verificare se una data è successiva o precedente rispetto a un'altra, calcolare il numero di giorni tra due date o semplicemente stampare la data più recente o più vecchia. In questo articolo, impareremo come confrontare due date utilizzando il linguaggio di programmazione C.

## Come fare

Per confrontare due date, dovremo utilizzare la struttura di dati `struct`. Non preoccuparti se non sei completamente familiare con le strutture in C, ti guideremo passo dopo passo attraverso un esempio pratico.

Iniziamo definendo la nostra struttura data che conterrà il giorno, il mese e l'anno:

```C
struct Data {
    int giorno;
    int mese;
    int anno;
};
```

Ora, dichiariamo due variabili di tipo `Data`:

```C
struct Data primaData, secondaData;
```

Possiamo assegnare dei valori alle variabili utilizzando l'operatore di assegnazione `=`:

```C
primaData.giorno = 18;
primaData.mese = 7;
primaData.anno = 2021;

secondaData.giorno = 10;
secondaData.mese = 7;
secondaData.anno = 2021;
```

Per confrontare queste due date, dovremo utilizzare l'operatore di confronto `>` (maggiore) o `<` (minore). Ad esempio, per verificare se la prima data è successiva rispetto alla seconda, possiamo scrivere:

```C
if (primaData.anno > secondaData.anno) {
    printf("La prima data è successiva alla seconda\n");
} else if (primaData.anno == secondaData.anno && primaData.mese > secondaData.mese) {
    printf("La prima data è successiva alla seconda\n");
} else if (primaData.anno == secondaData.anno && primaData.mese == secondaData.mese && primaData.giorno > secondaData.giorno) {
    printf("La prima data è successiva alla seconda\n");
} else {
    printf("La prima data è precedente alla seconda\n");
}
```

Questo codice controlla prima gli anni, poi i mesi e infine i giorni delle due date per determinare quale data è successiva. Naturalmente, questo esempio è basato sul supposto che le date siano inserite correttamente e siano valide.

Per calcolare il numero di giorni tra due date, possiamo creare una funzione che accetta due parametri di tipo `Data` e restituisce un valore intero rappresentante il numero di giorni. Ad esempio:

```C
int calcolaNumeroGiorni(struct Data data1, struct Data data2) {
    int giorni1 = data1.anno * 365 + data1.mese * 30 + data1.giorno;
    int giorni2 = data2.anno * 365 + data2.mese * 30 + data2.giorno;
    return abs(giorni1 - giorni2);
}
```

Questa funzione utilizza una semplice formula per calcolare il numero di giorni totali di una data e quindi restituisce il valore assoluto della differenza tra i due valori.

## Approfondimento

Ci sono alcune cose da tenere a mente quando si lavora con date in C. In primo luogo, il formato di data predefinito è `mese/giorno/anno`. Inoltre, è importante ricordare che alcuni anni non sono bisestili e quindi non hanno un giorno in più come il 29 febbraio. Infine, puoi anche utilizzare la libreria `time.h` per lavorare con date in C.

## Vedi anche

- [Documentazione sulla struttura di dati `struct` in C](https://www.tutorialspoint.com/cprogramming/c_structures.htm)
- [Tutorial su come utilizzare la libreria `time.h` in C](https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html)
- [Codice sorgente completo di esempio su GitHub](https://github.com/esempio/confronta-date-c)