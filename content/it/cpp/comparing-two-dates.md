---
title:    "C++: Confrontare due date."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché confrontare due date in C++

Il confronto tra due date è un'operazione molto comune in programmazione, in particolare nel linguaggio C++. Sapere come confrontare correttamente le date può aiutarci a gestire in modo efficiente le nostre applicazioni e a ottenere i risultati desiderati. In questo articolo, esamineremo come effettuare un confronto tra due date utilizzando il linguaggio C++.

## Come fare

Per confrontare due date in C++, abbiamo bisogno di utilizzare la libreria standard di C++ "ctime", che ci fornisce le funzioni necessarie per lavorare con le date. Inoltre, avremo bisogno di dichiarare una struttura "tm" che rappresenta una data.

Per iniziare, dobbiamo inizializzare due variabili di tipo "tm" con le due date che vogliamo confrontare. Ad esempio, supponiamo che vogliamo confrontare le date 5/10/2021 e 10/15/2021. Dobbiamo dichiarare due variabili di tipo "tm" e assegnare loro i valori delle date:

```C++
tm data1, data2;

data1.tm_mday = 5; //giorno
data1.tm_mon = 9; //mese - 1 (ottobre è il mese 9)
data1.tm_year = 121; //anno - 1900 (2021 - 1900 = 121)

data2.tm_mday = 15; //giorno
data2.tm_mon = 9; //mese - 1 (ottobre è il mese 9)
data2.tm_year = 121; //anno - 1900 (2021 - 1900 = 121)
```

Ora che abbiamo inizializzato le due variabili con le date desiderate, possiamo utilizzare la funzione "difftime" della libreria "ctime" per confrontarle. Questa funzione calcola la differenza in secondi tra le due date e restituisce un valore. Se il valore è maggiore di 0, allora la prima data è successiva alla seconda data; se è minore di 0, allora la prima data è precedente alla seconda data.

```C++
double diff = difftime(mktime(&data1), mktime(&data2)); //confronto tra le due date
//mktime converte una variabile tm in una data del calendario
cout << "Differenza in secondi: " << diff << endl;
```

In questo caso, la differenza è di -1296000 secondi, quindi la prima data è precedente alla seconda data.

## Approfondimento

È importante notare che quando confrontiamo due date, dobbiamo assicurarci che siano rappresentate nella stessa scala di tempo. Per esempio, se stiamo confrontando una data nel formato "gg/mm/aaaa", dobbiamo assicurarci che entrambe le date siano in questo formato. Inoltre, dobbiamo tenere conto dei diversi metodi di rappresentazione delle date nei vari paesi, ad esempio il formato "mm/gg/aaaa" negli Stati Uniti.

Un altro aspetto importante da considerare è il fatto che le date possono essere rappresentate in diversi formati, come ad esempio una stringa o un intero. In questi casi, dobbiamo utilizzare le funzioni appropriate per convertire la data nel formato "tm" prima di confrontarle.

## Vedi anche

- [Funzione difftime in C++](https://www.cplusplus.com/reference/ctime/difftime/)
- [Libreria standard di C++](https://www.cplusplus.com/reference/)