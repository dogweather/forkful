---
title:    "C++: Utskrift av felsökningsdata"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att skriva debugutskrifter är en viktig del av programmering eftersom det hjälper utvecklare att identifiera och lösa fel i koden. Genom att skriva ut variabler, värden och steg i koden kan man enkelt följa hur programmet körs och se var eventuella problem uppstår.

## Hur man gör

För att skriva en debugutskrift i C++ används vanligtvis standard biblioteksfunktionen "cout" från <iostream>. Det finns också andra sätt att skriva ut, som till exempel "printf" från <stdio.h>, men vi kommer att fokusera på "cout" i denna artikel.

För att skriva ut en textstring använder man "cout" följt av ett <<
tecken och sedan texten som ska skrivas ut inom citattecken. Om man vill skriva ut flera saker i samma utskrift använder man flera << tecken för att separera dem. Här är ett enkelt exempel:

```C++

#include <iostream>

using namespace std;

int main() {
  // Skriver ut text och värdet av variabeln "x"
  int x = 3;
  cout << "Variabeln x har värdet: " << x << endl;
  // Skriver ut flera variabler och en sträng
  int y = 5;
  int z = x * y;
  cout << "Produkten av x och y är: " << z << ", x = " << x << " och y = " << y << endl;
}

```

Detta kommer att ge följande utskrift:

```
Variabeln x har värdet: 3
Produkten av x och y är: 15, x = 3 och y = 5
```

Som du kan se använde vi "endl" för att lägga till en radbrytning mellan varje utskrift, men man kan också använda "\n" för att göra samma sak.

Det finns också andra användbara saker man kan skriva ut, som till exempel böjningar av variabler och olika datatyper. Här är ett annat exempel där vi skriver ut olika saker och visar hur man formaterar utskriften:

```C++

#include <iostream>

using namespace std;

int main() {
  // Skriver ut en flyttal med 2 decimaler
  float f = 3.1415;
  cout << "Pi är: " << fixed << setprecision(2) << f << endl;
  // Skriver ut en sträng med citeringstecken
  string str = "Programmering \"kan\" vara kul!";
  cout << "En sträng med citeringstecken: " << str << endl;
  // Skriver ut siffror med tusentalsavgränsare
  int num = 1234567;
  cout << "Ett nummer med tusentalsavgränsare: " << fixed << setprecision(2) << num << endl;
}

```

Detta kommer att ge följande utskrift:

```
Pi är: 3.14
En sträng med citeringstecken: Programmering "kan" vara kul!
Ett nummer med tusentalsavgränsare: 1,234,567.00
```

## Djupdykning

Att skriva debugutskrifter kan vara till stor hjälp när man utvecklar och fixar fel i sin kod, men det är också viktigt att inte överanvända det. För mycket utskrifter kan göra koden svår att läsa och förstå, och det är oftast bättre att använda debugger-verktyg för att analysera variabler och körningssteg.

Om man vill vara mer organisatorisk med sina debugutskrifter kan man använda "cerr" istället för "cout". "cerr" skriver ut direkt till felspårningskonsolen och kan vara särskilt användbart vid felsökning av program som körs på en server.

Och kom ihåg, när du är klar med din felsökning, se till att ta bort eller kommentera ut dina debugutskrifter för att undvika onödig utskrift i produktionskoden.

## Se även

- [