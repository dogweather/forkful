---
title:                "Analisi di una data da una stringa"
html_title:           "C++: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Cos'è e perché?

Il parsing di una data da una stringa è un'operazione comune per i programmatori che permette di estrarre una data da una stringa di testo. Questo è utile quando si devono manipolare delle date in un programma, ad esempio per convertirle in un formato diverso o per effettuare calcoli su di esse.

# Come fare:

Per eseguire il parsing di una data da una stringa in C++, è possibile utilizzare la libreria `chrono` e la classe `istringstream`. Iniziamo includendo le relative header:

```C++
#include <chrono>
#include <sstream>
```

Supponiamo di avere una stringa contenente una data in formato "dd/mm/yyyy" e che vogliamo estrarre questa data come oggetto `chrono::system_clock::time_point`. Possiamo fare così:

```C++
std::string dataString = "11/05/2020";
std::istringstream iss(dataString);
std::tm dataStruct = {};
iss >> std::get_time(&dataStruct, "%d/%m/%Y");
auto data = std::chrono::system_clock::from_time_t(std::mktime(&dataStruct));
```

Il nostro oggetto `data` ora conterrà la data del nostro stringa. Possiamo anche stamparla in un formato diverso utilizzando la classe `strftime`:

```C++
// per esempio, come "11 MAy 2020"
std::time_t dataT = std::chrono::system_clock::to_time_t(data);
char dataFormatted[80];
std::strftime(dataFormatted, 80, "%d %b %Y", std::localtime(&dataT));
std::cout << dataFormatted;
```

# Approfondimenti:

Il parsing di una data da una stringa è un'operazione che risale ai primi tempi della programmazione. In passato, si utilizzavano metodi più manuali per convertire una data in un formato specifico, come ad esempio la suddivisione della stringa in sottostringhe e la loro conversione in numeri interi. Oggi, invece, con l'utilizzo di apposite librerie e classi, è diventato un processo molto più semplificato.

Esistono anche altre alternative per estrarre una data da una stringa, come l'utilizzo di espressioni regolari o l'utilizzo di librerie esterne specializzate in parsing di date. Tuttavia, l'utilizzo di `chrono` e `istringstream` è uno dei metodi più comuni e semplici in C++.

Per quanto riguarda l'implementazione dell'operazione di parsing di una data da una stringa, è importante tenere conto delle eccezioni e dei possibili errori che possono verificarsi durante il processo. Ad esempio, se la stringa non rispetta il formato specificato, il programma potrebbe generare un'eccezione. Inoltre, è consigliabile gestire in modo appropriato eventuali errori di conversione o di allocazione della memoria.

# Vedi anche:

- Documentazione sulla libreria `chrono`: https://en.cppreference.com/w/cpp/chrono
- Documentazione sulla classe `istringstream`: https://en.cppreference.com/w/cpp/io/basic_istringstream
- Esempi di parsing di una data da una stringa in C++: https://www.fluentcpp.com/2020/07/03/converting-string-numbers-dates-time-objects-cpp/#dates-and-time
- Tutorial su come utilizzare espressioni regolari in C++: https://www.learncpp.com/cpp-tutorial/regular-expressions/