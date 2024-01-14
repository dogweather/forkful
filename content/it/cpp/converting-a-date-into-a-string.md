---
title:    "C++: Convertire una data in una stringa"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, mentre si lavora con i dati, è necessario convertire una data in una stringa per scopi di presentazione o salvataggio. Per questo, è importante sapere come farlo in modo efficiente e accurato utilizzando il linguaggio di programmazione C++.

## Come Convertire una Data in una Stringa in C++

Per convertire una data in una stringa in C++, è necessario utilizzare la libreria standard <ctime> che fornisce diverse funzioni per gestire le date e gli orari.

Il primo passo è dichiarare una variabile di tipo struct tm che rappresenta una struttura per contenere una data e un orario. Successivamente, si utilizza la funzione std::time() che restituisce il tempo corrente in secondi dal 1 gennaio 1970. Questo valore viene poi passato alla funzione std::localtime() che converte il tempo in una data e la memorizza nella struct tm.

```C++
#include <iostream>
#include <ctime>

int main() {
    //dichiarazione della variabile struct tm 
    struct tm * timeinfo;
    //acquisizione del tempo corrente
    time_t current_time = std::time(0);
    //conversione del tempo in una data
    timeinfo = std::localtime(&current_time);
    //utilizzo delle funzioni per ottenere i singoli componenti della data
    int day = timeinfo->tm_mday;
    int month = timeinfo->tm_mon + 1;
    int year = timeinfo->tm_year + 1900;
    //creazione della stringa della data nel formato desiderato
    std::string date_str = std::to_string(day) + "/" + std::to_string(month) + "/" + std::to_string(year);
    //stampa della data convertita
    std::cout << date_str << std::endl;
    return 0;
}
```

In questo esempio, si utilizzano le funzioni std::to_string() per convertire i numeri dei componenti della data in stringhe. Si noti che è necessario aggiungere 1 al mese e 1900 all'anno per ottenere il valore corretto.

L'output sarà qualcosa del tipo "13/08/2021" a seconda della data corrente.

## Approfondimenti 

La funzione std::strftime() può essere utilizzata per ottenere una maggiore flessibilità nella conversione di una data in una stringa. Essa permette di specificare il formato desiderato della stringa di output, ad esempio "dd/mm/yyyy" o "mm/dd/yyyy". Inoltre, è possibile utilizzare la funzione std::put_time() per ottenere un formato personalizzato senza dover specificare i singoli componenti della data.

## Vedi Anche

- Documentazione ufficiale di <ctime> in C++: https://en.cppreference.com/w/cpp/header/ctime
- Guida alla gestione delle date e degli orari in C++: https://www.learncpp.com/cpp-tutorial/102-basic-inputoutput/