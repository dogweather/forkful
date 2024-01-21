---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:45:24.400898-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Estrarre sottosequenze di stringhe significa prelevare parti di testo da una stringa più grande. Questo è utile per analizzare i dati, manipolare il testo e permettere agli algoritmi di lavorare solo con le informazioni necessarie.

## How to: (Come fare:)
```C++
#include <iostream>
#include <string>

int main() {
    std::string frase = "Ciao, mondo della programmazione!";
    std::string sottosequenza = frase.substr(6, 5); // partendo da indice 6, lunghezza 5
    std::cout << sottosequenza << std::endl; // Output: mondo

    // Un altro esempio con la ricerca di un indice
    size_t pos = frase.find("programmazione");
    if (pos != std::string::npos) {
        std::string parola = frase.substr(pos);
        std::cout << parola << std::endl; // Output: programmazione!
    }

    return 0;
}
```

## Deep Dive (Approfondimento)
Estrarre sottosequenze è un'operazione comune nella programmazione sin dall'inizio. Nel C++, questo si fa con `std::string::substr()`, disponibile dalla libreria standard. Alternative includono il taglio di stringhe con funzioni `c_str()` o `strcpy()` del C, ma con C++ si tende a preferire `substr()` per la sicurezza di tipo e per evitare errori di buffer overflow.

Dettagli implementativi: `substr()` crea una nuova stringa e può lanciare `std::out_of_range` se si tenta di accedere a caratteri fuori dai limiti della stringa originale. Quando si usa `find()`, `std::string::npos` rappresenta un valore costante che indica la non presenza della sottosequenza. È importante gestire questo caso per evitare errori.

## See Also (Vedi Anche)
- Documentazione ufficiale su `std::string::substr`: https://en.cppreference.com/w/cpp/string/basic_string/substr
- Tutorial su `std::string`: https://www.cplusplus.com/reference/string/string/
- Articolo sulla sicurezza delle operazioni sulle stringhe in C++: https://www.owasp.org/index.php/C++_String_Manipulation