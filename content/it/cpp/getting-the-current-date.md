---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in C++

## Che cos'è e perché?

Ottenere la data corrente in C++ significa recuperare la data dell'orario corrente del tuo sistema. E' uno strumento utile per timestamp log, o tracciare eventi temporali specifici nel tuo programma.

## Ecco come si fa:

C++11 ha introdotto la libreria chrono, rendendo più semplice l'ottenimento della data corrente. Ecco un breve esempio:

```C++
#include <chrono>
#include <iostream>
#include <ctime>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t currentTime = std::chrono::system_clock::to_time_t(now);
    
    std::cout << "La data corrente è: " << std::ctime(&currentTime) << "\n";

    return 0;
}
```

Nell'esecuzione del programma, l'output sarà come:

```
La data corrente è: Thu Jun 20 14:30:10 2023
```

## Approfondimento

Prima di C++11, ottenere la data corrente richiedeva l'uso di funzioni C, che possono essere più complesse da utilizzare. `-<ctime>` fornisce la funzione `time()`, che può essere utilizzata per ottenere il tempo corrente. Tuttavia, la libreria chrono offre un'interfaccia più moderna e sicura.

Esistono anche alternative a chrono, come Boost DateTime, ma queste richiedono librerie esterne e quindi potrebbero non essere ideali per tutti i progetti.

In termini di funzionamento interno, `std::chrono::system_clock::now()` interagisce con l'orologio del sistema sottostante per ottenere l'ora corrente. La funzione è portabile e dovrebbe funzionare su tutti i sistemi moderni.

## Vedi anche

Per saperne di più sulla manipolazione del tempo in C++, dai un'occhiata a questi link:

1. [cppreference: chrono](https://en.cppreference.com/w/cpp/chrono)
2. [cplusplus.com: ctime](http://www.cplusplus.com/reference/ctime/)
3. [Boost: DateTime](https://www.boost.org/doc/libs/1_67_0/doc/html/date_time.html)