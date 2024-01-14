---
title:    "C++: Omvandla en sträng till gemener"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Det är vanligt i programmering att behöva konvertera en sträng till små bokstäver, även kallat "lower case". Detta kan behövas av olika anledningar, till exempel för att jämföra strängar eller för att upprätthålla en enhetlig kodstandard. I denna bloggpost kommer vi att utforska hur man konverterar en sträng till små bokstäver i C++.

## Så här gör du
För att konvertera en sträng till små bokstäver i C++ kan vi använda funktionen `tolower()` från standardbiblioteket `<cctype>`. Detta förutsätter att teckenuppsättningen som används är ASCII. Om vi vill kunna hantera andra teckenuppsättningar kan vi använda den mer mångsidiga funktionen `std::transform()` från biblioteket `<algorithm>`.

Först behöver vi inkludera rätt bibliotek och skapa en sträng som vi vill konvertera:

```C++
#include <iostream>
#include <string>
#include <cctype> // innehåller tolower()
#include <algorithm> // innehåller std::transform()

std::string str = "Ett Sträng";
```

För att konvertera strängen till små bokstäver med `tolower()`, kan vi använda en for-loop för att gå igenom varje tecken i strängen och tillämpa funktionen på det:

```C++
for (int i = 0; i < str.length(); i++) {
    str[i] = tolower(str[i]);
}
```

Alternativt, om vi vill använda `std::transform()`, kan vi använda en lambda-funktion som gör att vi enkelt kan tillämpa `tolower()` på varje tecken:

```C++
std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c) {
    return std::tolower(c);
});
```

Efter detta steg kommer vår sträng att vara i små bokstäver. För att se resultatet kan vi skriva ut strängen i konsolen:

```C++
std::cout << str; // "ett sträng"
```

## Djupdykning
Att konvertera en sträng till små bokstäver kan verka som en enkel uppgift, men det finns några saker att tänka på. Som nämnts ovan fungerar `tolower()` bara med ASCII-teckenuppsättningar. Om vi vill hantera andra teckenuppsättningar, såsom Unicode, behöver vi använda en annan metod eller ett tredjepartsbibliotek.

En annan viktig sak att notera är effektiviteten hos de olika metoderna. Att använda en for-loop kan vara enklare att förstå och göra för mindre strängar, men för större mängder data, kan `std::transform()` vara snabbare. Det finns också andra sätt att konvertera en sträng till små bokstäver, såsom att använda bitmanipulering eller använda den inbyggda funktionen `std::tolower()` i stället för `tolower()`.

Slutligen är det viktigt att tänka på effekterna av att ändra en sträng permanent. Om vi vill behålla den ursprungliga strängen kan vi skapa en kopia och applicera konverteringen på den i stället.

## Se även
- [C++ Standardbibliotek referens - tolower()](https://www.cplusplus.com/reference/cctype/tolower/)
- [C++ Standardbibliotek referens - std::transform()](https://www.cplusplus.com/reference/algorithm/transform/)
- [Utforska C++ Strängar](https://www.learncpp.com/cpp-tutorial/6-11-problems-with-c-style-strings-and-how-to-solve-them/)