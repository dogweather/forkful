---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenazione di stringhe in C++

## Cos'è e Perché?
La concatenazione di stringhe è l'atto di unire due o più stringhe in una sola. È un'operazione comune e molto utile per la manipolazione di testo nei programmi, come la creazione di messaggi personalizzati o di uscite formattate.

## Come si fa:
In C++, esistono molteplici metodi per concatenare stringhe. Ecco alcuni esempi.

```C++
#include <string>
#include <iostream>

int main() {
    std::string saluto = "Ciao, ";
    std::string nome = "Carlo!";
    std::string messaggio = saluto + nome; // Concatenazione
    std::cout << messaggio; // Stampa: Ciao, Carlo!
}

```
Questo esempio concatena semplicemente due stringhe usando l'operatore `+`.

```C++
#include <string>
#include <iostream>

int main() {
    std::string saluto = "Ciao, ";
    saluto += "Carlo!"; // Concatenazione con +=
    std::cout << saluto; // Stampa: Ciao, Carlo!
}
```
Questo esempio utilizza l'operatore `+=` per concatenare una stringa direttamente ad un'altra.

## Approfondimento
Prima dell'introduzione del tipo `std::string` in standard C++, la concatenazione di stringhe era un'operazione più laboriosa, tipicamente fatta con array di caratteri e funzioni come `strcat`.

Un'alternativa alla concatenazione di stringhe con l'operatore `+` o `+=` è l'utilizzo di `std::stringstream` o `fmt::format` (dalla libreria {fmt} e disponibile nel C++20).

Rispetto alle stringhe concatenate manualmente, questi metodi offrono più controllo sul formato del testo finale.

## Vedere Anche
Per maggiori informazioni sulla concatenazione di stringhe in C++, consulta i seguenti link:

- [Stringhe in C++ su cplusplus.com](http://www.cplusplus.com/reference/string/string/)
- [String concatenation: The good, the bad, and the ugly](https://www.fluentcpp.com/2017/01/20/the-most-important-thing-about-string-concatenation-in-cpp/)
- [Documentazione su std::stringstream](http://www.cplusplus.com/reference/sstream/stringstream/)
- [Guide alla libreria {fmt}](https://fmt.dev/latest/index.html)