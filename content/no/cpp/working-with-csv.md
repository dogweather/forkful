---
title:                "Å arbeide med csv"
html_title:           "C++: Å arbeide med csv"
simple_title:         "Å arbeide med csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data, er sjansen stor for at du må håndtere CSV-filer. CSV står for "Comma Separated Values" og er en enkel måte å lagre og utveksle tabellariske data på.

## Slik gjør du det

Å jobbe med CSV-filer i C++ er en enkel prosess. Først må du inkludere <fstream> biblioteket som lar deg lese og skrive filer. Deretter kan du bruke ifstream og ofstream klassene for å lese og skrive til CSV-filer.

```C++
#include <fstream>

int main() {
    std::ifstream input("data.csv");
    int num;
    while (input >> num) {
        // les data fra filen
    }

    std::ofstream output("resultat.csv");
    output << "1,2,3\n";
    output << "4,5,6\n";
    output.close();
}
```

## En dypere dykk

Når du arbeider med CSV-filer, er det viktig å være oppmerksom på noen utfordringer. Pass på at filen er korrekt formatert og at du håndterer spesielle tegn som komma eller anførselstegn. Det kan også være lurt å inkludere bibliotek for databehandling, som for eksempel Boost eller Qt.

## Se også

- [Boost C++ Libraries](https://www.boost.org/)
- [Qt Framework](https://www.qt.io/)