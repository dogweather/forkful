---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre det første bokstaven i hvert ord stort, ofte brukt for titler eller for å markere begynnelse av setninger. Programmerere bruker dette til å standardisere tekstdata og forbedre lesbarheten.

## Hvordan gjøre det:
```java
import java.util.Arrays;
import java.util.stream.Collectors;

public class CapitalizeString {

    public static String capitalizeWords(String input) {
        if (input.isEmpty()) {
            return input;
        }
        return Arrays.stream(input.split("\\s"))
                .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase())
                .collect(Collectors.joining(" "));
    }

    public static void main(String[] args) {
        String text = "java er gøy, ikke sant?";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText);
    }
}
```
Output:
```
Java Er Gøy, Ikke Sant?
```

## Dypdykk
Før Unicode og internasjonalisering ble vanlig, var det å kapitalisere en streng mye enklere. I tidligere programmeringsspråk, med mindre tegnsett, utførte man enkel byte-manipulasjon. For eksempel, i ASCII er det en kjent forskjell på 32 desimaler mellom en liten og stor bokstav, noe som gjorde oppgaven triviell.

Alternativer inkluderer bruk av `Character` klassen eller Apache Commons' `StringUtils`. I Java 8 og oppover er det mer vanlig å bruke `streams` for eleganse og lesbarhet.

Implementasjonsdetaljer: Det er viktig å huske lokale forskjeller ved kapitalisering, som i Turkisk hvor 'i' blir 'İ' når den er kapitalisert. Java's `toUpperCase()` tar hensyn til lokalisering hvis du bruker den med locale-spesifikke overbelastninger.

## Se Også
- Oracle's Java documentation on `String`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html
- Apache Commons Lang StringUtils, an elegant external library for string manipulation: https://commons.apache.org/proper/commons-lang/
