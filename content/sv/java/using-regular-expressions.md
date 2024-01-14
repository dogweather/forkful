---
title:                "Java: Användning av reguljära uttryck"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions, även kallat regex, är ett användbart verktyg för att söka och ersätta textsträngar i Java. Det är särskilt användbart för att hitta mönster eller utföra komplexa operationer på stora datamängder.

## Hur man använder det
För att använda regular expressions i Java behöver du importera klassen `java.util.regex`, som innehåller metoder för att skapa mönster och utföra sökningar.

För att skapa ett mönster, använd `Pattern.compile()` och ange den reguljära uttrycksträngen som du vill matcha.

```Java
Pattern pattern = Pattern.compile("[aeiou]");
```

För att söka efter en match i en textsträng, använd `matcher() `och ange textsträngen som ska matchas.

```Java
Matcher matcher = pattern.matcher("Hello World");
```

För att hitta alla förekomster av mönstret, använd `find()` tills det returnerar `false`.

```Java
while(matcher.find()){
  System.out.print(matcher.group() + " ");
}
// Output: e o o
```

## Djupdykning
Det finns många olika metoder i `java.util.regex` som kan användas för att söka, matcha, ersätta och manipulera textsträngar.

Några användbara metoder inkluderar `matches()`, som jämför hela textsträngen med mönstret, `replaceAll()` och `replaceFirst()`, som ersätter matchade strängar med en annan sträng, och `split()`, som delar upp textsträngen baserat på mönstret.

Det finns också olika specialtecken som kan användas för att söka efter specifika mönster, till exempel `.` för att matcha vilket tecken som helst och `[]` för att ange möjliga teckenalternativ.

## Se också
- Java dokumentation för `java.util.regex`: https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html
- Regex tutorial på svenska: https://www.lifewire.com/begreppen-bakom-reguljara-uttryck-i-java-2034123
- Regex Cheatsheet: https://www.rexegg.com/regex-quickstart.html