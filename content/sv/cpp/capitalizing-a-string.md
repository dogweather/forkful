---
title:    "C++: Att göra en sträng stor"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför
Att använda en huvudbokstav i en sträng kan vara användbart för att ge strukturen åt en text som visas för användaren. Det gör det också enklare att urskilja namn och titlar.

## Hur man gör
För att göra en huvudbokstav i en sträng i C++ kan du använda funktionen "toupper()" tillsammans med en for-loop som itererar över varje tecken i strängen. Här är ett exempel på hur det kan se ut:

```C++
#include <iostream>
#include <string>

int main() {

  // Skapa en ny sträng som ska kapitaliseras
  std::string str = "hej alla vänner";

  // Loopa igenom varje tecken i strängen
  for (int i = 0; i < str.length(); i++) {

    // Om tecknet är en bokstav, gör om den till en stor bokstav
    if (isalpha(str[i])) {
      str[i] = toupper(str[i]);
    }
  }

  // Skriv ut den kapitaliserade strängen
  std::cout << str << std::endl;

  return 0;
}
```

### Resultat:
```
HEJ ALLA VÄNNER
```

## Djupdykning
Förutom att bara konvertera en sträng till stora bokstäver, kan man även använda denna teknik för att göra första bokstaven i varje ord stor. Detta kan åstadkommas genom att kontrollera varje teckes position i förhållande till mellanslag. Man kan också ha koll på specialtecken som punkt och frågetecken för att se till att varje mening börjar med en stor bokstav.

Det finns även inbyggda funktioner i C++ som kan göra detta enklare, såsom "capitalize()", "toupper()" och "tolower()". Genom att utforska dessa funktioner kan man hitta en lämplig metod för sitt specifika användningsområde.

## Se även
- [C++ string functions](https://www.geeksforgeeks.org/c-string-functions/)
- [C++ Character Handling](https://www.tutorialspoint.com/cplusplus/cpp_character_handling.htm)
- [capitalize function reference](http://www.cplusplus.com/reference/cctype/capitalize/)