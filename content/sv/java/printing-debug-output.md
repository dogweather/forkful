---
title:                "Java: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att skriva ut felsökningsutdata är ett vanligt sätt för utvecklare att identifiera och åtgärda problem i deras kod. Genom att utskriftsprocessen kan man se exakt vad som händer vid varje steg i programmet och därmed enklare hitta fel och missar.

## Hur man gör
För att skriva ut felsökningsutdata i Java, kan man använda sig av metoden "System.out.println()" som skriver ut en given sträng eller variabel till konsolen. Här kommer ett exempel:

```java
public class DebugPrinter {

    public static void main(String[] args) {

        // Skapa en variabel att skriva ut
        String message = "Hej, världen!";

        // Skriv ut variabeln
        System.out.println(message);

        // Använd också felsökningsutdata för att kontrollera variabler
        int number1 = 5;
        int number2 = 10;
        int sum = number1 + number2;
        System.out.println("Summan av " + number1 + " och " + number2 + " är " + sum);
    }

}
```
Output:
```
Hej, världen!
Summan av 5 och 10 är 15
```
Här ser vi hur vi kan skriva ut en enkel sträng samt använda felsökningsutdata för att kontrollera vad som händer med variabler i en beräkning.

## Djupdykning
Att skriva ut felsökningsutdata kan vara ett effektivt sätt att förstå vad som händer i ens program, men det är också viktigt att använda den rätt. Här kommer några tips för att få ut mesta möjliga av felsökningsutdata i Java:

- Använd tydliga och beskrivande meddelanden för att förstå vad som skrivs ut.
- Undvik att skriva ut stora mängder data som kan göra konsolen ogenomtränglig.
- Kombinera felsökningsutdata med andra verktyg som loggar eller debugger för att få en mer omfattande bild av ditt program.
- Kom ihåg att ta bort eller kommentera ut alla felsökningsutdata innan du släpper din kod till en produktionsmiljö.

## Se även
- [Java API-dokumentation](https://docs.oracle.com/javase/8/docs/api/)
- [Debugging in Java: A Short Introduction](https://www.baeldung.com/java-debugging)
- [Java Debugging with Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2017/february/article2.php)