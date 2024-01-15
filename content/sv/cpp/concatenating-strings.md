---
title:                "Sammanslående av strängar"
html_title:           "C++: Sammanslående av strängar"
simple_title:         "Sammanslående av strängar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig uppgift inom programmering när man behöver kombinera olika textsträngar för att skapa en större och mer komplex sträng. Det kan vara användbart när man bygger användargränssnitt, loggar eller hanterar data från flera olika källor.

## Hur man gör det

För att sammanslå strängar behöver vi använda oss av en operator som kallas för "concatenation" eller "sammanfogning". I C++ kan vi använda en plus-tecken (+) för att sammanslå två eller flera strängar. Se nedan för ett exempel:

```C++
#include <iostream>

int main() {

    // Två strängar som vi vill sammanslå
    std::string str1 = "Välkommen ";
    std::string str2 = "till C++ världen!";

    // Så här kan vi använda plus-tecknet för att sammanslå strängarna
    std::string resultat = str1 + str2;

    // Skriv ut resultatet för att se den sammanfogade strängen
    std::cout << resultat << std::endl;

    return 0;
}

// Output:
// Välkommen till C++ världen!
```

I det här exemplet sammanslår vi två strängar för att skapa en ny sträng "resultat". Genom att använda operatorn + så slås de två strängarna ihop och resultatet blir en ny sträng som innehåller båda orden tillsammans.

Det är också möjligt att sammanslå flera strängar på en gång genom att använda + flera gånger. Se nedan för ett exempel:

```C++
#include <iostream>

int main() {

    // Tre strängar som vi vill sammanslå
    std::string str1 = "Svenska";
    std::string str2 = " är ett";
    std::string str3 = " roligt språk!";

    // Nya strängar kommer att läggas till en efter en för att sammanslå dem alla
    std::string resultat = str1 + str2 + str3;

    // Skriv ut resultatet
    std::cout << resultat << std::endl;

    return 0;
}

// Output:
// Svenska är ett roligt språk!
```

## Djupdykning

När man sammanslår strängar i C++, bör man vara medveten om att det finns en bättre och mer effektiv metod för att göra detta, som kallas "string stream". Detta är särskilt användbart när man arbetar med ett större antal strängar som behöver sammanslås.

En string stream skapar en temporär sträng där man kan lägga till strängar och annan data. När man är klar med detta kan man omvandla string streamen till en vanlig sträng genom att använda funktionen "str()" och sedan använda den som en vanlig sträng. Se nedan för ett exempel:

```C++
#include <iostream>
#include <sstream> // inkludera string stream biblioteket

int main() {

    // Tre strängar som vi vill sammanslå
    std::string str1 = "I ";
    std::string str2 = "älskar ";
    std::string str3 = "C++!";

    // Skapa en string stream
    std::stringstream sstr;

    // Lägg till de tre strängarna till string streamen
    sstr << str1 << str2 << str3;

    // Omvandla till en vanlig sträng och spara det i en variabel
    std::string resultat = sstr.str();

    // Skriv ut resultatet
    std::cout << resultat << std::endl;

    return 0;
}

// Output:
// I älskar C++!
```

Det här är bara en grundläggande introduktion till sammanslagning av strängar i C++. Det finns många olika användningsområden och metoder för att göra detta, så fortsätt att experimentera och lära dig mer.

## Se även

- [C++ Strings](https://www.learncpp.com