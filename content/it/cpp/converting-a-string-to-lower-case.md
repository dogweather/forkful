---
title:                "C++: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui uno potrebbe voler convertire una stringa in caratteri minuscoli in programmazione. Questa operazione è particolarmente utile per confrontare e confrontare stringhe, nonché per cercare e sostituire determinati caratteri.

## Come Fare
Per eseguire questa operazione in C++, è necessario utilizzare la funzione `tolower()` che converte un singolo carattere in minuscolo. Tuttavia, poiché desideriamo convertire l'intera stringa, dobbiamo iterare attraverso ogni singolo carattere e applicare la funzione `tolower()` ad ognuno di essi. Ecco un esempio di codice e il relativo output:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "QUESTO È UNA STRINGA IN MAIUSCOLO";
    cout << "Stringa originale: " << str << endl;

    for (int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    cout << "Stringa convertita in minuscolo: " << str << endl;
    return 0;
}
```

Output:
```
Stringa originale: QUESTO È UNA STRINGA IN MAIUSCOLO
Stringa convertita in minuscolo: questo è una stringa in minuscolo
```

## Approfondimento
Nella maggior parte dei casi, la funzione `tolower()` funzionerà correttamente per convertire una stringa in minuscolo. Tuttavia, ci sono alcune eccezioni a questa regola a causa delle diverse codifiche di caratteri utilizzate nei diversi sistemi operativi.

Ad esempio, le lettere accentate come "à", "è" o "ù" non verranno convertite correttamente utilizzando solo la funzione `tolower()`. Invece, è necessario utilizzare una libreria aggiuntiva come `iconv` per gestire correttamente queste eccezioni.

Inoltre, è importante notare che la funzione `tolower()` non modifica la stringa originale, ma crea una nuova stringa convertita. Se si desidera modificare effettivamente la stringa originale, è necessario utilizzare la funzione `transform()`.

## Vedi Anche
- [Funzione `tolower()` in C++](https://www.cplusplus.com/reference/cctype/tolower/)
- [Manipolazione di stringhe in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Libreria `iconv` in C++](https://www.gnu.org/software/libiconv/)