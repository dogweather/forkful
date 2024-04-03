---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:45.795609-07:00
description: "Att g\xF6ra f\xF6rsta bokstaven i varje ord i en str\xE4ng till versal\
  \ inneb\xE4r att man modifierar f\xF6rsta bokstaven till att vara stor bokstav medan\
  \ resten g\xF6r man\u2026"
lastmod: '2024-03-13T22:44:37.770988-06:00'
model: gpt-4-0125-preview
summary: "Att g\xF6ra f\xF6rsta bokstaven i varje ord i en str\xE4ng till versal inneb\xE4\
  r att man modifierar f\xF6rsta bokstaven till att vara stor bokstav medan resten\
  \ g\xF6r man till gemener."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Vad & Varför?
Att göra första bokstaven i varje ord i en sträng till versal innebär att man modifierar första bokstaven till att vara stor bokstav medan resten gör man till gemener. Denna vanliga manipulering av strängar är användbar för att formatera text i applikationer, till exempel för att förbereda användarnamn eller titlar för visning enligt konvention eller grammatisk korrekthet.

## Hur:
Javas standardbibliotek erbjuder inte en direkt metod för att göra hela strängar versaler på en gång, men detta kan åstadkommas med en kombination av inbyggda metoder. För mer sofistikerade behov erbjuder tredjepartbibliotek som Apache Commons Lang enkla lösningar.

### Använda Javas inbyggda metoder
För att göra en sträng versal utan externa bibliotek kan du dela upp strängen i ord, göra första bokstaven i varje ord versal och sedan sätta ihop dem igen. Här är ett enkelt tillvägagångssätt:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Skriver ut: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Denna kodsnutt konverterar hela strängen till gemener och itererar sedan genom varje tecken, och gör första bokstaven i varje ord till en versal. Det betraktar blanksteg, punkter och apostrofer som ordavgränsare.

### Använda Apache Commons Lang

Apache Commons Lang-biblioteket erbjuder en mer elegant lösning med metoden `WordUtils.capitalizeFully()`, som hanterar olika specialfall och avgränsare åt dig:

```java
// Lägg till beroende: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Skriver ut: "Hello, World!"
    }
}
```

För att använda denna metod behöver du lägga till Apache Commons Lang-biblioteket i ditt projekt. Denna biblioteksmetod gör inte bara första bokstaven i varje ord till en versal utan konverterar också resten av bokstäverna i varje ord till gemener, vilket säkerställer ett konsekvent mönster av versalisering genom hela strängen.
