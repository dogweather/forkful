---
title:                "Utskrift av felsökningstexter"
html_title:           "Java: Utskrift av felsökningstexter"
simple_title:         "Utskrift av felsökningstexter"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man, som utvecklare, skulle vilja skriva ut debug-meddelanden i sin Java-kod. Det kan hjälpa till att förstå hur koden fungerar, hitta och åtgärda fel och ge en bättre överblick över programmets flöde.

## Hur

För att skriva ut debug-meddelanden i Java använder man en metod som heter "println()". Detta är en inbyggd metod i Java som tar emot en sträng som argument och skriver ut den till konsolen. Här är ett exempel på hur man skulle kunna använda den:

```Java
int age = 25;
System.out.println("Min ålder är " + age);
```

Detta kommer att skriva ut "Min ålder är 25" till konsolen. Man kan också använda "print()" metoden, som gör samma sak som "println()" utan att lägga till en radbrytning efter. Detta kan vara användbart om man vill skriva ut flera meddelanden på samma rad. Här är ett exempel:

```Java
System.out.print("Hej ");
System.out.print("Världen!");
```

Detta kommer att skriva ut "Hej Världen!" på samma rad.

## Djupdykning

När det kommer till att skriva ut debug-meddelanden är det viktigt att hitta en balans mellan att ha tillräckligt med information för att förstå vad som händer i koden, samtidigt som det inte blir för mycket och rörigt. En bra strategi är att endast skriva ut viktiga variabler och steg i kodflödet. Man kan också använda olika nivåer av debuggning, som t.ex. "DEBUG", "INFO" och "ERROR", för att indikera vilken typ av meddelande det är. Detta kan hjälpa till att enklare hitta och sortera meddelandena.

## Se även

- [Java: Using System.out for debugging](https://www.baeldung.com/java-debugging-system-out)
- [Debugging in Java](https://docs.oracle.com/javase/7/docs/technotes/tools/windows/java.html#options)