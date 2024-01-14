---
title:                "Java: Sammanslagning av strängar"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en grundläggande men viktig del av programmering. Genom att kombinera flera strängar till en kan du skapa mer dynamiska och komplett utskrifter. Det är en användbar kunskap som kan hjälpa dig att skriva mer effektiv och läsbar kod.

## Hur man gör det

Att sammanslå strängar är relativt enkelt i Java. Det finns flera sätt att göra det på, men det vanligaste är genom att använda metoden `concat()` eller operatorn `+`. Här är ett exempel på hur du skulle göra det med `concat()`:

```Java
String firstName = "Johan";
String lastName = "Andersson";
String fullName = firstName.concat(lastName);

System.out.println(fullName);
```

Output:

```
JohanAndersson
```

Här är samma exempel med `+` operatorn:

```Java
String firstName = "Johan";
String lastName = "Andersson";
String fullName = firstName + lastName;

System.out.println(fullName);
```

Output:

```
JohanAndersson
```

Som du kan se ger båda metoderna samma resultat. Det viktigaste är att du förstår konceptet bakom sammanslagning av strängar.

## Djupdykning

När du använder `concat()` metoden, skapas en ny sträng som innehåller de två ursprungliga strängarna. Det betyder att både `firstName` och `lastName` fortsätter att existera som separata strängar.

När det kommer till operatorn `+`, sker en automatisk konvertering av datatypen till `StringBuilder`, vilket gör det lättare och mer effektivt att sammanslå flera strängar.

Det finns också andra metoder för att sammanslå strängar, som `StringBuilder` och `StringBuffer`, vilket kan vara användbart för mer komplexa manipulationer av strängar.

## Se också

- [Java String Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java StringBuilder Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- [Java StringBuffer Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuffer.html)