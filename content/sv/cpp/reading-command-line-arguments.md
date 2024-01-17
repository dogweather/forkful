---
title:                "Läsning av kommandoradsargument"
html_title:           "C++: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument är processen att ta emot och tolka inmatade värden från användaren via kommandoraden i ett program. Detta är en viktig aspekt av programmering eftersom det ger en flexibilitet och interaktivitet till programmen. Genom att läsa kommandoradsargument kan programmet anpassas och utföra olika operationer baserat på användarens input.

## Så här:
Det finns flera sätt att läsa kommandoradsargument i C++, men det vanligaste är att använda funktionen `main()` tillsammans med två parametrar: `argc` och `argv`. `argc` representerar antalet argument som skickats medan `argv` är en vektor av strängar som innehåller de faktiska argumenten.

```C++
int main(int argc, char *argv[]) {
  // Kod för att läsa kommandoradsargument
  return 0;
}
```

Ett vanligt sätt att använda kommandoradsargument är att utföra olika operationer på en fil beroende på de argument som skickas med. Till exempel kan vi läsa in argumentet `filename` och använda det för att öppna och läsa en specifik fil:

```C++
int main(int argc, char *argv[]) {
  if (argc > 1) {
    // Läser in filnamnet från första argumentet (ignorerar programmets namn)
    string filename = argv[1];
    // Kod för att öppna och läsa filen
  } else {
    // Om inget argument skickas med
    cout << "Inget filnamn angivet!" << endl;
  }
  return 0;
}
```

När programmet körs, kan användaren nu ange ett filnamn som ska öppnas som ett kommandoradsargument:

```
$ ./programnamn fil.txt
```

## Djupdykning:
Kommandoradsargument har funnits sedan de tidigaste datorerna, då det var det enda sättet att interagera med datorn. Idag finns det dock alternativ till att läsa kommandoradsargument, som till exempel att använda en grafisk användargränssnitt (GUI) för att interagera med programmet.

Att läsa kommandoradsargument kan också vara en viktig säkerhetsåtgärd. Genom att kontrollera och filtrera de inmatade värdena kan man förhindra skadlig kod från att köras på datorn.

## Se även:
För mer information om att läsa kommandoradsargument i C++, se följande länkar:

- [Tutorialspoint - C++ Command Line Arguments](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [CPlusPlus - Command line arguments](https://www.cplusplus.com/articles/DEN36Up4/)
- [GeeksforGeeks - Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)