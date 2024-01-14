---
title:                "C++: Läsning av kommandoradsargument"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför 

Kommandoradsargument, även kallade "command line arguments" på engelska, är en viktig del av C++ programmering. Genom att läsa och använda kommandoradsargument kan du skapa mer flexibla och anpassningsbara program som kan ta emot olika indata från användaren.

## Hur man gör det

Att läsa kommandoradsargument är faktiskt ganska enkelt i C++. Allt du behöver göra är att använda den inbyggda "main" funktionen och dess "argc" och "argv" parametrar. Här är ett exempel på hur du kan göra det:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
  for (int i = 0; i < argc; i++) {
    std::cout << "Argument " << i + 1 << ": " << argv[i] << std::endl;
  }
  return 0;
}
```

I detta exempel skriver vi ut alla kommandoradsargument som användaren matar in. "argc" parametern håller antalet argument som skickas till programmet och "argv" parametern innehåller själva argumenten som en vektor av strängar. Genom att använda en "for" loop kan vi gå igenom alla argument och skriva ut dem.

Om vi skulle köra detta program med följande kommandon i kommandoraden:

```
./program argument1 argument2 argument3
```

Så kommer outputen att se ut så här:

```
Argument 1: ./program
Argument 2: argument1
Argument 3: argument2
Argument 4: argument3
```

Som du kan se inkluderas även programmet självt som det första argumentet.

## Djupdykning

Förutom att bara skriva ut kommandoradsargumenten kan du också använda dem för att göra din kod mer dynamisk och anpassningsbar. Genom att läsa in olika argument kan du till exempel ändra programmets beteende eller utföra olika uppgifter baserat på användarens indata.

Du kan också använda externa bibliotek som "boost program_options" för att göra hantering av kommandoradsargumenten ännu enklare och mer strukturerad.

## Se även

- [cppreference - Argumente från kommandoraden](https://en.cppreference.com/w/cpp/language/main_function)
- [Boost Program_options guide (på engelska)](https://www.boost.org/doc/libs/1_74_0/doc/html/program_options.html)