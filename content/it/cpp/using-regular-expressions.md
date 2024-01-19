---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Usare le Espressioni Regolari in C++
Una rapida panoramica dell'utilizzo delle espressioni regolari in C++ in uno stile semplice e non verboso.

## Cos'è? & Perché?
Le espressioni regolari (regex) sono strumenti potenti per la manipolazione delle stringhe. Aiutano i programmatori a trovare, sostituire, suddividere o combinare stringhe utilizzando pattern specifici.

## Come si fa:
Ecco un esempio di base di come usare le espressioni regolari in C++:

```C++
#include <regex>
#include <iostream>

int main(){
    std::string testo = "Hello 123, Ciao 456";
    std::regex pattern(R"(\b(\w+)\b)");

    std::sregex_iterator inizio(testo.begin(), testo.end(), pattern);
    std::sregex_iterator fine;

    for(std::sregex_iterator i = inizio; i != fine; ++i){
        std::cout<<(*i)[1]<<'\n';
    }
    return 0;
}
```

L'output di questo programma saranno le parole nel testo che combaciano con il pattern (Dove \w indica una qualunque “parola”):

```C++
Hello
123
Ciao
456
```

## Approfondimento 
Le espressioni regolari hanno origine dalle teorie matematiche degli automi e della linguistica formale. In C++, la libreria regex è stata aggiunta con lo standard C++11.

Esistono anche alternative, come le funzioni di manipolazione delle stringhe della libreria standard del C++, ma le espressioni regolari offrono maggiore flessibilità e potenza.

L'implementazione delle regex in C++11 non è locale. Ciò significa che i nomi dei caratteri e le loro categorie, utilizzati negli insiemi di caratteri delle espressioni regolari, si basano sulle impostazioni di localizzazione C.

## Vedi Anche
Per approfondimenti sulle espressioni regolari:

- Documentazione completa sulle regex di C++: http://www.cplusplus.com/reference/regex/
- Tutorial su regex basati su vari esempi: https://www.regular-expressions.info/tutorial.html
- Un interprete online di regex per provare i tuoi pattern: https://regex101.com/
- Libreria regex Boost per espressioni regolari avanzate: https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html