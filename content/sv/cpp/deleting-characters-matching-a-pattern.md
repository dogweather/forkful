---
title:                "C++: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Varför

Ibland behöver du kanske ta bort vissa tecken från en textsträng i ditt C++ program. Det kan vara för att rensa bort onödiga tecken eller för att filtrera ut specifika teckenmönster. Oavsett anledning så är det viktigt att veta hur man gör det på rätt sätt för att undvika fel och buggar.

# Hur man gör det

För att ta bort tecken som matchar ett visst mönster från en textsträng i C++ så kan du använda dig av en kombination av `find` och `erase` funktionerna. `Find` letar efter förekomster av ett specifikt teckenmönster och returnerar indexet av första förekomsten. `Erase` tar sedan bort tecknen från det angivna indexet. Till exempel, låt oss säga att vi har en textsträng och vill ta bort alla förekomster av bokstaven "a":

```C++
// Skapa en textsträng
string text = "Denna text innehåller flera bokstäver a";

// Hitta första förekomsten av "a"
int index = text.find("a");

// Om indexet inte är -1, fortsätt ta bort förekomsten av "a"
while (index != -1) {
    text.erase(index, 1);
    index = text.find("a");
}

// Skriv ut den nya textsträngen utan bokstaven "a"
cout << text << endl;

// Output: Denn text innehller fler bokstver
```

Det är viktigt att notera att när vi tar bort tecken från en textsträng så förskjuts alla tecken till vänster efter det borttagna tecknet. Det betyder att indexet för nästa förekomst också kommer att förskjutas. Därför behöver vi använda en loop för att fortsätta hitta och ta bort alla förekomster tills det inte finns några kvar.

# Djupdykning

Om du vill ta bort tecken från en textsträng men även behålla en del av tecknen från ett visst index, kan du använda `replace` funktionen istället för `erase`. `Replace` tar förutom index och antal tecken att ta bort, även ett antal tecken att lägga till på samma index. Till exempel, om vi vill ta bort alla förekomster av "a" men behålla bokstaven "ä" vid index 10, då kan vi använda följande kod:

```C++
// Skapa en textsträng
string text = "Denna text innehåller flera bokstäver a";

// Hitta första förekomsten av "a"
int index = text.find("a");

// Om indexet inte är -1, fortsätt ersätta tecknet med "ä" samtidigt som vi tar bort "a"
while (index != -1) {
    text.replace(index, 1, "ä");
    index = text.find("a");
}

// Skriv ut den nya textsträngen
cout << text << endl;

// Output: Denna text innehäller flera bokstäver ä
```

Detta är en mer avancerad teknik som kan vara användbar i vissa situationer, men kräver lite extra planering och tänkande.

# Se även

- [C++ dokumentation för `find`](https://cppreference.com/w/cpp/string/basic_string/find)
- [C++ dokumentation för `erase`](https://cppreference.com/w/cpp/string/basic_string/erase)
- [C++ dokumentation för `replace`](https://cppreference.com/w/cpp/string/basic_string/replace)

Hoppas denna guide har hjälpt dig att förstå hur man tar bort tecken som matchar ett visst mönster i C++. Lycka till med dina programmeringsprojekt!