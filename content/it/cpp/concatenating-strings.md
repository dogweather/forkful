---
title:                "Concatenazione di stringhe"
html_title:           "C++: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una tecnica molto utile per un programmatore C++. Permette di combinare diverse stringhe in un'unica stringa, consentendo di creare un output unico e personalizzato nel nostro codice.

## Come fare

Per concatenare stringhe in C++ è necessario utilizzare l'operatore "+" o la funzione "concat" (nel caso di stringhe di oggetti standard). Di seguito è riportato un esempio di codice:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string nome = "Marco";
  string cognome = "Rossi";
  string nome_completo = nome + " " + cognome;
  cout << "Il tuo nome completo è: " << nome_completo << endl;
  return 0;
}
```

L'output di questo esempio sarà:

```
Il tuo nome completo è: Marco Rossi
```

Un'altra opzione per eseguire la concatenazione di stringhe è utilizzare la funzione "concat" dei classi stringa. L'esempio seguente mostra come utilizzare questa funzione:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str1 = "Ciao";
  string str2 = "Mondo";
  string str3 = str1.concat(str2);
  cout << "La stringa concatenata è: " << str3 << endl;
  return 0;
}
```

L'output di questo esempio sarà:

```
La stringa concatenata è: CiaoMondo
```

## Deep Dive

La concatenazione di stringhe in C++ può diventare molto utile quando si lavora con grandi quantità di dati e richiede operazioni efficienti. Quando si utilizza l'operatore "+", è importante tenere presente che possono verificarsi problemi di prestazioni quando si concatenano stringhe di grandi dimensioni. In questi casi, è preferibile utilizzare la funzione "concat" dei classi stringa poiché offre prestazioni migliori.

## Vedi anche

- [Documentazione di concat in C++](https://www.geeksforgeeks.org/concat-function-in-cpp/)
- [Tutorial sulle stringhe in C++](https://www.programiz.com/cpp-programming/strings)