---
title:    "C++: Sammanslagning av strängar"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att slå ihop strängar (engelska: concatenating strings) är en viktig del av programmering eftersom det tillåter dig att skapa mer dynamiska och flexibla applikationer. Genom att kombinera flera strängar kan du skapa mer komplexa uttryck och anpassa ditt program efter olika situationer.

## Så här gör du

För att slå ihop strängar i C++ behöver du använda en speciell operator som heter " + ". Denna operator tillåter dig att kombinera två strängar och skapa en ny, längre sträng.

Låt oss titta på ett exempel:

```C++
#include <iostream>
using namespace std;

int main() {
    // Definiera två variabler med strängar
    string förnamn = "Anna";
    string efternamn = "Andersson";

    // Slå ihop strängarna och lagra resultatet i en ny variabel
    string namn = förnamn + " " + efternamn;

    // Skriv ut resultatet
    cout << "Namn: " << namn;

    return 0;
}

// Output: Namn: Anna Andersson
```

Som du kan se i exemplet ovan kombinerade vi två strängar "förnamn" och "efternamn" med hjälp av " + "-operatorn. Observera att vi även använde en extra sträng med ett mellanslag mellan förnamn och efternamn för att skapa en korrekt namnsträng.

## Djupdykning

När du slår ihop strängar i C++ så görs det genom att skapa en ny string-objekt som har en längre storlek än de enskilda strängarna. Detta görs automatiskt av kompilatorn, så du behöver inte oroa dig för det.

En viktig sak att komma ihåg är att om du vill uppdatera en sträng efter att den redan har skapats måste du använda funktionen "append()". Detta gör det möjligt att lägga till strängar till en befintlig sträng utan att skapa en ny.

## Se även

- [C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)
- [String Concatenation in C++](https://www.geeksforgeeks.org/concatenation-of-strings-in-cpp/)
- [C++ String Manipulation](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_manipulation.htm)

## Se även

- [C++ Strängar](https://www.w3schools.com/cpp/cpp_strings.asp)
- [Strängslagning i C++](https://www.geeksforgeeks.org/concatenation-of-strings-in-cpp/)
- [C++ Strängmanipulering](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_manipulation.htm)