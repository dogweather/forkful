---
title:                "Unendo le stringhe"
html_title:           "C++: Unendo le stringhe"
simple_title:         "Unendo le stringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché

Concatenare le stringhe significa unire due o più stringhe in una sola. I programmatori spesso lo fanno per costruire testi dinamici o per manipolare dati di stringhe. Ad esempio, se si vuole stampare un messaggio personalizzato per un utente, è necessario concatenare le stringhe per unire il nome dell'utente con il resto del messaggio.

## Come si fa

Nel linguaggio di programmazione C++, è possibile concatenare le stringhe con l'operatore `+` o utilizzando la funzione `std::string::append()`. Ecco un esempio di entrambi i metodi:

```C++
#include <iostream>
#include <string>

int main() {

  // utilizzando l'operatore +
  std::string nome = "Mario";
  std::string messaggio = "Ciao, " + nome + "! Benvenuto nel nostro sito!";
  std::cout << messaggio << std::endl;

  // utilizzando la funzione append()
  std::string mese = "marzo";
  std::string data = "La data di oggi è ";
  data.append(mese);
  std::cout << data << std::endl;

  return 0;
}
```

Risultato:

```
Ciao, Mario! Benvenuto nel nostro sito!
La data di oggi è marzo
```

## Approfondimento

Nella programmazione, le stringhe sono rappresentate da una successione di caratteri e possono essere gestite in vari modi, a seconda del linguaggio utilizzato. In C++, le stringhe possono essere trattate come oggetti grazie alla classe `std::string`, che fornisce metodi utili per manipolare le stringhe, come la funzione `append()` utilizzata nell'esempio precedente. Un'altra opzione per concatenare le stringhe in C++ è utilizzare la funzione `std::to_string()` per convertire un valore numerico in una stringa e poi utilizzare l'operatore `+` per unirle.

## Vedi anche

Per ulteriori informazioni sull'utilizzo delle stringhe in C++, consulta la documentazione ufficiale di [std::string](https://en.cppreference.com/w/cpp/string/basic_string) e prova a risolvere questi [esercizi](https://www.hackerrank.com/domains/cpp?filters%5Bsubdomains%5D%5B%5D=strings).