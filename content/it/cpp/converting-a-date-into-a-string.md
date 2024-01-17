---
title:                "Convertire una data in una stringa"
html_title:           "C++: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?
Convertire una data in una stringa significa trasformare un dato nel formato di una stringa che rappresenti la data. I programmatori spesso fanno questo per visualizzare le date in modo più leggibile per gli utenti o per lavorare con esse in modo più semplice nel codice.

## Come Fare:
```C++
#include<iostream>
#include<ctime>

using namespace std;

int main(){
    time_t now = time(0);
    char* date = ctime(&now);
    cout << "Data corrente: " << date <<endl;
    return 0;
}
```

**Output:**
```
Data corrente: Wed Oct 20 21:25:04 2021
```

## Approfondimento:
Convertire una data in una stringa è stato un problema comune per i programmatori nel passato, poiché le diverse lingue e le date in diversi formati rendevano difficile la rappresentazione accurata. Oggi, ci sono molte librerie, come Boost.DateTime, che semplificano il processo. Inoltre, è possibile utilizzare funzioni personalizzate per formattare la data in base alle esigenze dello sviluppatore.

## Vedi anche:
- [Boost.DateTime](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [C++ Reference - ctime](https://cplusplus.com/reference/ctime/)