---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:16.106858-07:00
description: "Hur man g\xF6r: Javas inbyggda st\xF6d f\xF6r regulj\xE4ra uttryck \xE4\
  r fr\xE4mst genom klasserna `Pattern` och `Matcher` i paketet `java.util.regex`.\
  \ H\xE4r \xE4r ett enkelt\u2026"
lastmod: '2024-03-13T22:44:37.777647-06:00'
model: gpt-4-0125-preview
summary: "Javas inbyggda st\xF6d f\xF6r regulj\xE4ra uttryck \xE4r fr\xE4mst genom\
  \ klasserna `Pattern` och `Matcher` i paketet `java.util.regex`."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:
Javas inbyggda stöd för reguljära uttryck är främst genom klasserna `Pattern` och `Matcher` i paketet `java.util.regex`. Här är ett enkelt exempel för att hitta och skriva ut alla förekomster av ett ord i en sträng, utan att skilja på stora och små bokstäver:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Hittade '" + matcher.group() + "' på position " + matcher.start());
        }
    }
}
```

Utmatning:
```
Hittade 'parsing' på position 16
Hittade 'Parsing' på position 31
```

För uppgifter som att dela upp strängar kan du använda klassen `String` och dess metod `split()` med ett reguljärt uttryck:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Utmatning:
```
Java
Python
Ruby
JavaScript
```

När du arbetar med reguljära uttryck i Java kan det finnas fall där ett externt bibliotek kan förenkla komplexa uppgifter. Ett av de populära tredjepartsbiblioteken för att arbeta med reguljära uttryck i Java är `Apache Commons Lang`. Det erbjuder verktyg som `StringUtils` som gör vissa uppgifter med reguljära uttryck mer rakt på sak. Så här använder du det för att räkna matchningar av en delsträng:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' förekommer " + count + " gånger.");
    }
}
```

För att använda Apache Commons Lang behöver du inkludera det i ditt projekt. Om du använder Maven, lägg till detta beroende i din `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Kolla efter senaste versionen -->
</dependency>
```

Utmatning:
```
'processing' förekommer 2 gånger.
```
