---
title:                "C++: Stora bokstäver i en sträng"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till stora bokstäver är en vanlig operation inom programmering. Det kan vara användbart för att göra texten mer läsbar eller för att jämföra strängar med lika bokstäver oavsett storlek.

## Hur man gör det

```C++
#include <iostream> //inkluderar standardbiblioteket för in- och utmatning
#include <string> //inkluderar biblioteket för strängar
using namespace std;

int main() {
    string str = "hej världen";
    for(int i=0;i<str.length();i++){ //loopar igenom varje tecken i strängen
        if (str[i]>=97 && str[i]<=122){ //kollar om tecknet är en liten bokstav
            str[i] = str[i]-32; //konverterar det till versal genom att subtrahera 32 från det
        }
    }
    cout << str; //skriver ut den konverterade strängen
    return 0;
}

```

Output:
```
HEJ VÄRLDEN
```

## Djupdykning

För att förstå varför vi behöver konvertera strängar till stora bokstäver måste vi förstå hur datorer lagrar bokstäver. I datorns interna kodning, ASCII, tilldelas varje bokstav ett numeriskt värde. Till exempel är bokstaven "a" i ASCII-koden 97 och "A" är 65. Det finns en skillnad på 32 mellan bokstäverna i versal och gemener.

När vi jämför två strängar i vår kod, kommer de att förstås som olika om ena är skriven med stora bokstäver och andra med små, även om de innehåller samma bokstäver. Därför är det nödvändigt att konvertera strängarna till en enhetlig form för att undvika felaktiga resultat.

## Se även

- [C++ strings](https://www.learncpp.com/cpp-tutorial/working-with-strings/)
- [ASCII table](https://www.asciitable.com/)