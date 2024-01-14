---
title:                "Java: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions, eller regelbundna uttryck, är ett kraftfullt verktyg som kan hjälpa utvecklare att söka och manipulera textdata. Genom att använda sig av ett speciellt syntax kan man söka efter specifika mönster i textsträngar och utföra olika åtgärder. Några av dessa användningsområden inkluderar validering av telefonnummer och e-postadresser, filter av data och parsning av loggfiler. Genom att använda regular expressions kan man spara tid och undvika en del manuellt arbete.

## Så här gör man
För att använda regular expressions i Java behöver man importera java.util.regex-paketet och skapa ett Regex-objekt som innehåller det mönster man vill söka efter. Det finns även olika metoder för att hitta eller ersätta matchande mönster i en textsträng. Nedan följer några vanliga exempel:

- Hitta ett ord i en textsträng: ```Java
String pattern = "hej";
String text = "Hej, hur mår du?";
Pattern regex = Pattern.compile(pattern);
Matcher matcher = regex.matcher(text);
boolean found = matcher.find(); // true
```

- Ersätta text i en sträng: ```Java
String newString = matcher.replaceAll("tja"); // Tja, hur mår du?
```

- Validera en e-postadress: ```Java
String pattern = "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$"; // ett mönster för en enkel e-postadress, inte helt komplett
String email = "test@test.com";
Pattern regex = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE);
Matcher matcher = regex.matcher(email);
boolean isValid = matcher.matches(); // true
```

- Plocka ut siffror från en text: ```Java
String pattern = "\\d+"; // ett mönster för att hitta siffror
String text = "Det var 10 söta katter";
Pattern regex = Pattern.compile(pattern);
Matcher matcher = regex.matcher(text);
while (matcher.find()) {
    System.out.println(matcher.group()); // 10
}
```

Det finns många olika möjligheter och olika syntaxer att använda sig av när det kommer till regular expressions. Det är viktigt att kolla på dokumentationen för att hitta rätt mönster för det man behöver göra.

## Djupdykning
Att lära sig och förstå regular expressions kan ta lite tid, men det är definitivt värt det. Genom att använda ett regular expression-chekverktyg som regex101.com kan man experimentera och testa olika mönster. Det finns även olika nätverk bibliotek som kan hjälpa till med att skriva och förstå regular expressions. Övning är verkligen nyckeln till att behärska regular expressions och det behövs en viss tidsinvestering för att verkligen bli bekväm med dem.

## Se även
- [The Java Tutorials - Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regex101.com](https://regex101.com/)
- [Java Regex Cheatsheet](https://zeroturnaround.com/rebellabs/java-regex-cheat-sheet/)