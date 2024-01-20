---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolazione delle stringhe in C++

## Che cosa & Perché?

L'interpolazione delle stringhe è l'inserimento di variabili nel mezzo del testo. I programmatori lo fanno per creare contenuti dinamici e migliorare la leggibilità del codice.

## Come fare:

C++ supporta l'interpolazione delle stringhe usando operatori come `+` e `<<`. Guardate il codice di seguito:

```C++
#include <iostream>
#include <string>

int main() {
    std::string nome = "Mario";
    int eta = 22;
    
    std::cout << "Ciao, mi chiamo " << nome << " e ho " << eta << " anni.\n";

    return 0;
}
```
Output:
```shell
Ciao, mi chiamo Mario e ho 22 anni.
```

## Approfondimento

Storicamente, C++ non supportava l'interpolazione delle stringhe, costringendo i programmatori ad usare concatenazione o formattazione tradizionale. 

Altre lingue di programmazione, come Python o JavaScript, hanno una sintassi integrata per l'interpolazione delle stringhe che rende il codice più pulito.

L'interpolazione avviene a tempo di esecuzione; questo significa che le espressioni immesse vengono valutate e gli eventuali calcoli necessari vengono effettuati al tempo dell'esecuzione.

Sebbene l'interpolazione delle stringhe sia comoda, tieni a mente che può portare ad alcune debolezze di sicurezza, specialmente quando si lavora con dati provenienti da fonti esterne. È sempre importante verificare ed eseguire la pulizia dei dati prima di utilizzarli.

## Vedi anche

- Documentazione ufficiale C++ (https://en.cppreference.com/)
- C++ String Interpolation (https://www.geekhideout.com/cppformat.shtml)
- Sicurezza dell'interpolazione delle stringhe (https://owasp.org/www-community/attacks/SQL_Injection)