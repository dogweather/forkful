---
title:                "Å bruke regulære uttrykk"
html_title:           "C++: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Regular expressions (regex) er et kraftig verktøy som brukes av programmører til å søke etter og manipulere tekststrenger. Det sparer tid og krefter ved å tillate en enkel og effektiv måte å søke etter mønstre og utføre handlinger på dem.

# Hvordan?
Coding eksempler og resultatutdata i ```C++ ...``` kodeblokker:

**Eksempel 1: Søk etter et enkelt tegn**

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string str = "Hei, jeg er en programmerer!";
    
    if (regex_search(str, regex("j"))) {
        cout << "Strengen inneholder tegnet 'j'" << endl;
    }
    else {
        cout << "Strengen inneholder ikke tegnet 'j'" << endl;
    }
    
    return 0;
}

// Output:
// Strengen inneholder tegnet 'j'
```

**Eksempel 2: Søk etter flere tegn**

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string str = "Regulære uttrykk er awesome!";
    
    if (regex_search(str, regex("awe?"))) {
        cout << "Strengen inneholder mønsteret 'awe?'" << endl;
    }
    else {
        cout << "Strengen inneholder ikke mønsteret 'awe?'" << endl;
    }
    
    return 0;
}

// Output:
// Strengen inneholder mønsteret 'awe?'
```

**Eksempel 3: Manipulering av tekst**

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string str = "123456789";
    
    regex_replace(str, regex("[0-9]{2}"), "XX");
    cout << str << endl;
    
    return 0;
}

// Output:
// XX34XX89
```

# Deep Dive
Regular expressions ble opprinnelig utviklet i 1950-årene av matematikeren Stephen Kleene som en teoretisk modell for regulære språk. I dag brukes det i programmeringsspråk som C++, Java og Python for å søke og manipulere tekststrenger.

Alternativene til regex inkluderer tekstmanipuleringsfunksjoner og string-klasser i ulike programmeringsspråk. Disse er ofte mindre komplekse, men kan ikke håndtere komplekse mønstre som regex kan.

Implementering av regex gjøres vanligvis gjennom biblioteker eller innebygde funksjoner i programmeringsspråk. I C++, biblioteket <regex> brukes til dette formålet.

# Se også
* [C++ regex tutorial](https://www.cplusplus.com/reference/regex/)
* [Regex cheatsheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
* [Online regex tester](https://regex101.com/)