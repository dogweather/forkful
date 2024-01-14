---
title:                "C++: Confrontare due date"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Spesso nel mondo della programmazione, ci troviamo nella necessità di confrontare due date. Questo potrebbe essere utile per controllare la validità di un'informazione, per gestire scadenze o per ordinare dati temporalmente. In questo articolo, impareremo come confrontare due date utilizzando il linguaggio di programmazione C++.

## Come fare

Per confrontare due date in C++, dobbiamo prima di tutto capire come rappresentare le date all'interno del codice. Una delle opzioni è quella di utilizzare la struttura di dati `struct tm` della libreria standard `ctime`.

Dichiariamo due variabili di tipo `tm` e assegniamo loro le rispettive date che vogliamo confrontare:

```C++
struct tm data1, data2;
data1.tm_mday = 5; //giorno
data1.tm_mon = 6; //mese (gennaio = 0, dicembre = 11)
data1.tm_year = 2020; //anno - 1900 (2020 - 1900 = 120)
data2.tm_mday = 10;
data2.tm_mon = 6;
data2.tm_year = 2020;
```

Ora che abbiamo dichiarato e inizializzato le due date, possiamo utilizzare la funzione `difftime` per calcolare la differenza in secondi tra le due date:

```C++
time_t differenza = difftime(mktime(&data2), mktime(&data1));
```

La funzione `mktime` convertirà la struttura `tm` in un tipo `time_t`, che rappresenta il numero di secondi trascorsi dal 1 gennaio 1970. In questo modo, avremo un risultato che sarà positivo se la prima data è precedente alla seconda, negativo se è viceversa, o nullo se le due date sono uguali.

Possiamo anche confrontare le date utilizzando l'operatore di confronto `>`, `<` o `==`:

```C++
if (data1 > data2) {
  cout << "La data 1 è successiva alla data 2." << endl;
} else if (data1 < data2) {
  cout << "La data 1 è precedente alla data 2." << endl;
} else {
  cout << "Le due date sono uguali." << endl;
}
```

## Approfondimento

Per gestire in modo più completo le date in C++, possiamo utilizzare la libreria `boost::gregorian`, che offre numerose funzioni per gestire le date, compresa la possibilità di effettuare operazioni come l'addizione o sottrazione di giorni, mesi o anni.

## Vedi anche

- https://www.cplusplus.com/reference/ctime/
- https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html