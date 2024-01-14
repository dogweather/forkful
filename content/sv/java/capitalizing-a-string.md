---
title:                "Java: Att göra en sträng med stor bokstav"
simple_title:         "Att göra en sträng med stor bokstav"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför

Att kapitalisera en sträng kan vara användbart för att förbättra utseendet och lättläsligheten i en applikation. Det är också ett vanligt krav i vissa system och API-anrop.

##Så här gör du

För att kapitalisera en sträng i Java kan du använda metoden `toUpperCase()` tillsammans med `charAt()` för att iterera igenom strängen. Nedan följer ett exempel på hur detta kan göras:

```Java
String str = "hej på dig";
String capitalizedStr = "";

for (int i = 0; i < str.length(); i++) {
    capitalizedStr += Character.toUpperCase(str.charAt(i));
}

System.out.println(capitalizedStr);
```

**Output:**

```
HEJ PÅ DIG
```

Detta är bara ett enkelt exempel, det finns flera andra sätt att kapitalisera en sträng i Java. Det är också viktigt att notera att Java är ett språk med strikt typtilldelning, så det är viktigt att ange datatypen för variablerna korrekt.

##Djupdykning

När man tittar närmare på hur metoden `toUpperCase()` fungerar, så används en intern algoritm för att konvertera varje bokstav till dess motsvarande versala form. Detta innebär att även specialtecken eller accenter kommer att kapitaliseras på rätt sätt.

Det finns också andra sätt att kapitalisera en sträng i Java, som att använda `StringBuilder` eller `StringBuffer` för en mer effektiv implementation. Det är viktigt att förstå hur olika metoder påverkar prestandan i en applikation.

##Se även

- [Java String Klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder Klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Java StringBuffer Klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)