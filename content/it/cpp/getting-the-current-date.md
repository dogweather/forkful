---
title:    "C++: Ottenere la data corrente"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ogni programma ha bisogno di uno strumento per tenere traccia del tempo, e ottenere la data corrente è un'operazione fondamentale in quasi ogni linguaggio di programmazione. In questo articolo, impareremo come ottenere la data corrente in C++, fornendo sia una semplice soluzione che una più approfondita.

## Come fare

Per ottenere la data corrente in C++, dobbiamo includere la libreria `ctime`, che ci fornisce funzioni per gestire il tempo. La funzione principale che useremo è `time()`, che restituisce il numero di secondi trascorsi dal 1 gennaio 1970 ad oggi.

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Otteniamo il numero di secondi trascorsi dall'1/1/1970 ad oggi
    std::time_t now = std::time(0);

    // Usiamo la funzione ctime per convertire il numero in una stringa
    std::cout << "La data corrente è: " << std::ctime(&now) << std::endl;

    return 0;
}
```

L'output sarà simile a questo:

```
La data corrente è: Mon Jul 12 11:30:38 2021
```

Possiamo anche manipolare la stringa ottenuta per ottenere solo la parte di data, ignorando l'ora e il fuso orario.

```C++
#include <iostream>
#include <ctime>

int main()
{
    std::time_t now = std::time(0);
    std::string dateString = std::ctime(&now);

    // Utilizziamo il metodo substring per ottenere la data
    std::cout << "La data corrente è: " << dateString.substr(0, 10) << std::endl;

    return 0;
}
```

L'output sarà:

```
La data corrente è: Mon Jul 12
```

## Approfondimento

Oltre alla funzione `time()`, possiamo anche utilizzare la struttura `tm` per ottenere informazioni più dettagliate sulla data corrente. Questa struttura include variabili come `tm_yday` (il giorno dell'anno), `tm_wday` (il giorno della settimana), `tm_mon` (il mese) e così via.

```C++
#include <iostream>
#include <ctime>

int main()
{
    std::time_t now = std::time(0);
    std::tm* timeInfo = std::localtime(&now);

    std::cout << "La data corrente è: " << timeInfo->tm_mon + 1 << "/" << timeInfo->tm_mday << "/" << timeInfo->tm_year + 1900 << std::endl;

    return 0;
}
```

L'output sarà:

```
La data corrente è: 7/12/2021
```

Ci sono anche altre funzioni utili nella libreria `ctime`, come ad esempio `asctime()` per formattare la stringa di data, `mktime()` per convertire la data in secondi e `strftime()` per formattare la data in modo personalizzato.

## Vedi anche

- [Documentazione ufficiale di C++ sulla libreria ctime](https://en.cppreference.com/w/cpp/header/ctime)
- [Tutorial su come ottenere la data corrente in C++](https://www.programiz.com/cpp-programming/library-function/ctime/time)