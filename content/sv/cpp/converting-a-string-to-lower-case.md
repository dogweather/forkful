---
title:                "Omvandla en sträng till gemener"
html_title:           "C++: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är användbart när du vill standardisera indata eller göra jämförelser mellan strängar oberoende av stora eller små bokstäver.

## Hur man gör
Det finns flera sätt att konvertera en sträng till små bokstäver i C++, men en vanlig metod är att använda standardfunktionen `tolower()` tillsammans med en `for`-loop.

```C++
string str = "SVENSKA";
for (int i = 0; i < str.length(); i++) {
    str[i] = tolower(str[i]);
}
cout << str << endl;
```
Output: svenska

Det finns också bibliotek som erbjuder funktioner för att konvertera strängar till små bokstäver, såsom `<algorithm>` och `<string>`.

## Deep Dive
Vid konvertering till små bokstäver finns det vissa saker att tänka på. Till exempel, i vissa språk kan vissa bokstäver ha olika teckenkoder för stora och små bokstäver, och det kan påverka resultatet av konverteringen. Det är också viktigt att veta att konvertering av en sträng till små bokstäver inte förändrar originalet, utan skapar en kopia av strängen med små bokstäver.

## Se även
- [C++ Standardbibliotek](https://en.cppreference.com/w/cpp/algorithm)
- [C++ Strängbibliotek](https://en.cppreference.com/w/cpp/string/basic_string)