---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:21.689540-07:00
description: "Regul\xE6re uttrykk (regex) i Java lar deg definere spesifikke m\xF8\
  nstre for \xE5 s\xF8ke, manipulere eller validere strenger i koden din. Programmerere\
  \ bruker dem\u2026"
lastmod: '2024-03-13T22:44:40.656335-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) i Java lar deg definere spesifikke m\xF8nstre\
  \ for \xE5 s\xF8ke, manipulere eller validere strenger i koden din. Programmerere\
  \ bruker dem\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Regulære uttrykk (regex) i Java lar deg definere spesifikke mønstre for å søke, manipulere eller validere strenger i koden din. Programmerere bruker dem for oppgaver som å parse loggfiler, validere brukerinndata, eller søke etter spesifikke mønstre innen tekst, noe som muliggjør sofistikert strengbehandling med minimal innsats.

## Hvordan:

Java sin innebygde støtte for regex er hovedsakelig gjennom `Pattern` og `Matcher` klassene i `java.util.regex` pakken. Her er et enkelt eksempel for å finne og skrive ut alle forekomster av et ord i en streng, uten å ta hensyn til store og små bokstaver:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String tekst = "Regex er flott for parsing. Parsing med regex er kraftfullt.";
        String ordÅFinne = "parsing";
        
        Pattern mønster = Pattern.compile(ordÅFinne, Pattern.CASE_INSENSITIVE);
        Matcher matcher = mønster.matcher(tekst);
        
        while (matcher.find()) {
            System.out.println("Fant '" + matcher.group() + "' på posisjon " + matcher.start());
        }
    }
}
```

Output:
```
Fant 'parsing' på posisjon 16
Fant 'Parsing' på posisjon 31
```

For oppgaver som å splitte strenger, kan du bruke `String` klassens `split()` metode med et regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String tekst = "Java,Python,Ruby,JavaScript";
        String[] språk = tekst.split(",");
        
        for (String språk : språk) {
            System.out.println(språk);
        }
    }
}
```

Output:
```
Java
Python
Ruby
JavaScript
```

Når du jobber med regex i Java, kan det være tilfeller der et eksternt bibliotek kan forenkle komplekse oppgaver. Ett av de populære tredjeparts bibliotekene for å jobbe med regex i Java er `Apache Commons Lang`. Det tilbyr hjelpeverktøy som `StringUtils` som gjør noen regex oppgaver enklere. Slik bruker du det til å telle matcher av en understreng:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String tekst = "Regex gjør tekstbehandling enklere. Behandling av tekst med regex er effektivt.";
        String understreng = "behandling";
        
        int antall = StringUtils.countMatches(tekst, understreng);
        System.out.println("'" + understreng + "' fremkommer " + antall + " ganger.");
    }
}
```

For å bruke Apache Commons Lang, må du inkludere det i prosjektet ditt. Hvis du bruker Maven, legg til denne avhengigheten i `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Sjekk for den nyeste versjonen -->
</dependency>
```

Output:
```
'behandling' fremkommer 2 ganger.
```
