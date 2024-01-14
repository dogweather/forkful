---
title:    "C++: Omvandling av en sträng till små bokstäver"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är ett vanligt problem som programmerare kan stöta på vid utveckling av C++ program. Denna funktion kan vara användbar för att normalisera indata, jämföra strängar och göra sökningar mer flexibla. Det är också en bra övning för nya programmerare att öva på grundläggande C++ funktioner.

## Hur man gör det

För att konvertera en sträng till gemener i C++, använder vi den inbyggda funktionen `tolower()` som finns i `<cctype>` biblioteket. Nedan är ett exempel på hur man kan använda denna funktion:

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
  string s = "Hej, SWEDISH READERS!";
  for (int i = 0; i < s.length(); i++) {
    s[i] = tolower(s[i]);
  }
  cout << s << endl;
  return 0;
}
```

Koden ovan först deklarerar en sträng `s` med blandade stora och små bokstäver. Sedan loopas varje tecken i strängen och konverteras till gemener med hjälp av `tolower()` funktionen. Slutligen skrivs den nya strängen ut på skärmen. Output för denna kod skulle vara `hej, swedish readers!`.

## Djupdykning

För att förstå hur `tolower()` funktionen fungerar, är det viktigt att veta att den tar emot en hel bokstav som indata och returnerar samma bokstav i gemener. Om indata är en icke-bokstavlig tecken, returnerar den bara samma tecken utan någon konvertering.

En annan viktig aspekt att tänka på är att `tolower()` funktionen bara fungerar på enstaka bokstäver och inte hela strängar. Det är därför vi behöver loopa igenom varje tecken i strängen och konvertera dem en åt gången.

Slutligen, det kan vara värt att notera att `tolower()` funktionen fungerar på både ASCII och Unicode tecken, vilket gör den användbar för olika språk och teckenuppsättningar.

## Se även

- [C++ String Functions](https://www.geeksforgeeks.org/c-string-functions/) - En lista över andra användbara strängfunktioner i C++.
- [How to Convert a String to Lowercase in C++](https://www.bitdegree.org/learn/cpp-convert-string-to-lowercase) - En annan guide för att konvertera en sträng till gemener i C++.
- [All About C++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm) - En djupdykning i C++ strängar och deras användning i programmering.