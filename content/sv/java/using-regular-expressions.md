---
title:    "Java: Användning av reguljära uttryck"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför använda regelbundna uttryck i Java?

Regelbundna uttryck är ett kraftfullt verktyg inom Java-programmering som gör det möjligt att hitta och manipulera text på ett flexibelt sätt. Med hjälp av regelbundna uttryck kan du enkelt söka igenom en sträng efter specifika mönster eller uttryck, vilket kan spara mycket tid och arbete. Så varför ska du använda dem? Låt oss ta en titt på hur det fungerar.

## Så här använder du regelbundna uttryck i Java

För att använda regelbundna uttryck i Java, måste du först importera den inbyggda "Pattern" och "Matcher" klasserna. Sedan kan du skapa ett uttryck som innehåller det mönster du vill söka efter, och sedan köra det mot en sträng med hjälp av en Matcher-objekt. Här är ett exempel:

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {

    public static void main(String[] args) {
        String text = "Detta är en text med numret 123456789";
        String regex = "(\\d{3})(\\d{3})(\\d{3})"; // Uttryck för att hitta ett nummer med 9 siffror i rad

        //Skapa ett Pattern-objekt
        Pattern pattern = Pattern.compile(regex);

        //Kör matchningen på strängen
        Matcher matcher = pattern.matcher(text);

        //Loopa igenom matchningarna och skriv ut dem
        while (matcher.find()) {
            System.out.println("Hittade matchning: " + matcher.group());
        }
    }
}

// Output: Hittade matchning: 123456789
```

Som du kan se behöver vi bara några få rader kod för att hitta och skriva ut ett specifikt mönster i en sträng med hjälp av regelbundna uttryck.

## Djupdykning i användningen av regelbundna uttryck

Regelbundna uttryck kan vara mycket mer komplexa än det enkla exemplet ovan. De har en mängd olika funktioner och specialtecken som kan användas för att matcha olika typer av mönster i en sträng. Till exempel kan du använda karaktärsklassen "[a-z]" för att matcha alla små bokstäver, eller "[A-Z]" för att matcha alla stora bokstäver. Du kan också använda specialtecken som "*" för att representera noll eller fler förekomster av ett tecken, eller "?" för att representera noll eller en förekomst av ett tecken.

Det finns också många olika användningsområden för regelbundna uttryck, som validering av användarinput, filtrering av text från en fil eller webbsida, eller extrahering av viss information från en sträng. Utöver Java, kan du också använda regelbundna uttryck i andra programmeringsspråk som Python, PHP och JavaScript.

## Se även

- [Java Regex Tutorial på Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Regex Cheatsheet på Medium](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)
- [RegExr - Interaktiv regelbundna uttryckstester](https://regexr.com/)