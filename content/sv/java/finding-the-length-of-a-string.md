---
title:                "Java: Att hitta längden på en sträng"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom programmering och används ofta för att kontrollera inmatade data eller för att manipulera text. Det är en grundläggande funktion som kan vara användbar för olika ändamål.

## Hur man gör

För att hitta längden på en sträng i Java kan du använda metoden `.length()`. Den här metoden returnerar antalet tecken i en sträng och använder syntaxen `strängnamn.length()`, där "strängnamn" är namnet på den sträng du vill hitta längden på.

```Java
String namn = "Anna";
int längd = namn.length();
System.out.println("Längden på strängen \"" + namn + "\" är " + längd + " tecken.");
```

Output:
```
Längden på strängen "Anna" är 4 tecken.
```

Du kan också använda `.length()` på en tom sträng, vilket kommer att returnera värdet 0.

```Java
String tom = "";
System.out.println("Längden på den tomma strängen är " + tom.length() + " tecken.");
```

Output:
```
Längden på den tomma strängen är 0 tecken.
```

## Djupdykning

Bakom kulisserna använder `.length()` metoden egenskapen `.length` från klassen `String`, vilket är en viktig del av Java's standard bibliotek. Denna egenskap är en av flera som är tillgängliga för att manipulera och analysera strängar.

En intressant sak att notera är att `.length()` metoden returnerar en `int` (heltal), vilket betyder att den inte kan hantera längder som är större än 2,147,483,647 tecken. Om du behöver hitta längden på en sträng som är större än det, måste du använda en alternativ metod.

## Se även

- [Java String API](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java Strings Tutorial](https://www.w3schools.com/java/java_strings.asp)
- [Java String Class](https://www.geeksforgeeks.org/java-string-class/)