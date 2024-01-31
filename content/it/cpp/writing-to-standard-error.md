---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
La scrittura su _standard error_ (stderr) è l'output di errore standard del tuo programma. I programmatori lo usano per segnalare messaggi di errore, separati dagli output normali (stdout), permettendo una migliore gestione e debug.

## How to:
Uso di cerr per scrivere su stderr:
```C++
#include <iostream>

int main() {
    std::cerr << "Questo è un messaggio di errore." << std::endl;
    return 0;
}
```
Output:
```
Questo è un messaggio di errore.
```

Redirezione dell'output di stderr a un file in terminale (shell):
```shell
./your_program 2> error_log.txt
```

## Deep Dive
Stderr è una pratica che risale ai primi giorni dell'informatica, introdotta per aiutare a distinguere l'output normale dagli errori. Alcune alternative includono l'uso di log files e librerie di logging dedicate per applicazioni più complesse. In C++, stderr è implementato come uno stream precaricato pronto all'uso.

## See Also
- [Standard streams - cppreference.com](https://en.cppreference.com/w/cpp/io/c)
- [Logging in C++](https://stackoverflow.com/questions/7963763/what-is-the-best-logging-library-for-c)
