---
title:                "Sammanslående strängar"
html_title:           "C++: Sammanslående strängar"
simple_title:         "Sammanslående strängar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Concatenation av strängar, också känd som sammanfogning av strängar, är en process inom programmering där man kombinerar flera strängar till en enda sträng. Detta kan vara användbart för att skapa dynamiska textutskrifter eller för att manipulera datatyper som inte är strängar. Programmerare använder detta för att göra sina program mer välstrukturerade och flexibla.

## Hur man:
```C++
#include <iostream>
using namespace std;

int main() {
    string fornamn = "Lisa";
    string efternamn = "Persson";

    // Konkatenering med + operatorn
    string fulltnamn1 = fornamn + " " + efternamn;

    // Konkatenering med append() funktionen
    string fulltnamn2 = fornamn;
    fulltnamn2.append(" ");
    fulltnamn2.append(efternamn);

    cout << fulltnamn1 << endl; // Utskrift: Lisa Persson
    cout << fulltnamn2 << endl; // Utskrift: Lisa Persson
    return 0;
}
```

## Fördjupning:
Concatenation kan spåras tillbaka till de tidigaste datorerna där det var vanligt att dela upp en enda sträng i flera mindre strängar för att spara utrymme. Idag används också andra metoder som strängformattering för att kombinera text. Implementationen av strängkonkatenering kan variera beroende på programmeringsspråk, men konceptet är detsamma.

## Se även:
Läs mer om konkatenering i C++: [String Concatenation in C++](https://www.geeksforgeeks.org/string-concatenation-in-cpp/)