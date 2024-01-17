---
title:                "Confrontare due date"
html_title:           "C++: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Confrontare due date è semplicemente il processo di confrontare due dati temporali in modo da determinare quale sia precedente o successivo all'altro. I programmatori spesso lo fanno per ordinare eventi in ordine cronologico o per verificare se una data è antecedente o seguente a un'altra.

## Come: 
Ecco un semplice esempio di codice in C++ per confrontare due date e stampare un messaggio in base al risultato:

```C++
#include <iostream>
#include <ctime>

int main()
{
  // definisci le date da confrontare
  std::tm date1 = {0, 0, 0, 12, 10, 121};
  std::tm date2 = {0, 0, 0, 3, 9, 102};
  
  // converti le date in secondi dalla mezzanotte del 1 gennaio 1970
  std::time_t time1 = std::mktime(&date1);
  std::time_t time2 = std::mktime(&date2);

  // confronta le date
  if(time1 > time2)
    std::cout << "La prima data è successiva alla seconda.";
  else if(time1 < time2)
    std::cout << "La prima data è precedente alla seconda.";
  else
    std::cout << "Le date sono uguali.";

  return 0;
}
```
Output: 
```
La prima data è successiva alla seconda.
```

## Approfondimento:
- Storicamente, i programmatori dovevano scrivere il proprio algoritmo per confrontare le date, ma ora ci sono molte librerie e funzioni built-in che semplificano il processo.
- Un modo alternativo di confrontare le date è utilizzare il formato "AAAAMMGG" (anno, mese, giorno) anziché il formato unix timestamp utilizzato nell'esempio sopra.
- In alcune situazioni, è importante tenere in considerazione anche il fuso orario quando si confrontano date.
- Per confrontare date più complesse (ad esempio, includendo anche l'ora e i minuti), potrebbe essere necessario utilizzare funzioni più avanzate e considerare la definizione di "precedente" e "successivo" in modo diverso.

## Vedi anche:
- Documentazione ufficiale di C++: https://www.cplusplus.com/reference/ctime/
- Tutorial su come confrontare efficientemente due date in C++: https://www.techiedelight.com/compare-two-dates-cpp/