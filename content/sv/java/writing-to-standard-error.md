---
title:                "Java: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error, även kallad stderr, är en viktig del av felsökning och debugging inom Java-programmering. Det ger dig möjlighet att skriva ut felmeddelanden och annan viktig information till error streamen istället för till standard output streamen.

## Hur man gör det
För att skriva till stderr i Java kan du använda "System.error" klassen tillsammans med "println" metoden för att skriva ut önskat meddelande. Här är ett exempel på hur det kan se ut i din kod:

```Java
System.err.println("Ett fel har inträffat.");
```

Detta kommer att skriva ut "Ett fel har inträffat." till error streamen när koden körs. Om du vill skriva ut mer information, som variabler eller stack traces, kan du använda "System.err.format" metoden tillsammans med formatters. Här är ett exempel på hur detta kan se ut:

```Java
String name = "John Doe";
int age = 30;
System.err.format("Namn: %s, Ålder: %d", name, age);
```

Detta kommer att skriva ut "Namn: John Doe, Ålder: 30" till error streamen. Kom ihåg att du kan använda detta för debugging och även för att ge användbara felmeddelanden till användare av din kod.

## Djupdykning
Den största skillnaden mellan standard output streamen och error streamen är att error streamen inte buffras. Det betyder att allt som skrivs till stderr kommer att skrivas ut direkt. Detta är användbart när du vill vara säker på att felmeddelanden inte missas, men det kan också leda till att felmeddelanden blandas ihop om flera processer skriver till stderr samtidigt.

Det är också viktigt att notera att System.out och System.err delar samma standard output stream, vilket betyder att de inte bör användas för olika ändamål. Om du vill skriva till stderr ska du alltid använda System.err klassen.

## Se även
- [Java Dokumentation: System klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [The Java Tutorials: Flöden för input och output](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
- [Java Code Geeks: System.out vs System.err in Java](https://www.javacodegeeks.com/2015/04/system-out-vs-system-err-in-java.html)