---
title:                "Java: Söka och byta ut text"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering. Genom att använda denna funktion kan du enkelt ändra eller uppdatera stora mängder text i din kod. Det är också ett bra sätt att snabbt hitta och åtgärda felaktig eller föråldrad kod.

## Så här gör du

För att söka och ersätta text i Java kan du använda den inbyggda metoden `.replace()`. Detta tar två parametrar: den text du vill byta ut och den nya texten du vill ersätta den med. Till exempel:

```java
String original = "Hej! Välkommen till min blogg!";
String updated = original.replace("Hej", "Hallå");
System.out.println(updated);
```

Detta kommer att skriva ut: `Hallå! Välkommen till min blogg!`

Du kan också använda `.replace()` för att ta bort viss text genom att byta ut den med en tom sträng. Till exempel:

```java
String original = "Det är fredag idag!";
String updated = original.replace("fredag", "");
System.out.println(updated);
```

Detta kommer att skriva ut: `Det är idag!`

## Fördjupning

För att söka och ersätta mer avancerade mönster i din kod kan du använda regular expressions (Regex). Dessa mönster gör det möjligt att hitta och ersätta text som är mer komplex än en enkel sträng.

För att använda Regex i Java, använd `Pattern` och `Matcher` klasserna. Här är ett exempel på hur du kan använda det för att byta ut alla siffror i en sträng med "X":

```java
String original = "Din lösenord är 1234";
Pattern pattern = Pattern.compile("\\d");
Matcher matcher = pattern.matcher(original);
String updated = matcher.replaceAll("X");
System.out.println(updated);
```

Detta kommer att skriva ut: `Ditt lösenord är XXXX`

## Se även

- Java String API: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Java Matcher API: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html