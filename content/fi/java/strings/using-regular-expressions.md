---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:22.880405-07:00
description: "Kuinka: Javan sis\xE4\xE4nrakennettu tuki regexille tapahtuu p\xE4\xE4\
  asiassa `Pattern`- ja `Matcher`-luokkien kautta `java.util.regex`-paketissa. T\xE4\
  ss\xE4 on\u2026"
lastmod: '2024-03-13T22:44:56.434617-06:00'
model: gpt-4-0125-preview
summary: "Javan sis\xE4\xE4nrakennettu tuki regexille tapahtuu p\xE4\xE4asiassa `Pattern`-\
  \ ja `Matcher`-luokkien kautta `java.util.regex`-paketissa."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Kuinka:
Javan sisäänrakennettu tuki regexille tapahtuu pääasiassa `Pattern`- ja `Matcher`-luokkien kautta `java.util.regex`-paketissa. Tässä on yksinkertainen esimerkki, jolla etsitään ja tulostetaan kaikki sanojen esiintymät merkkijonossa, välittämättä kirjainkoosta:

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
            System.out.println("Löydetty '" + matcher.group() + "' sijainnista " + matcher.start());
        }
    }
}
```

Tuloste:
```
Löydetty 'parsing' sijainnista 16
Löydetty 'Parsing' sijainnista 31
```

Tehtäviin, kuten merkkijonojen jakamiseen, voit käyttää `String`-luokan `split()`-metodia regexin kanssa:

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

Tuloste:
```
Java
Python
Ruby
JavaScript
```

Työskennellessäsi regexien kanssa Javassa saattaa olla tapauksia, joissa ulkopuolinen kirjasto voi yksinkertaistaa monimutkaisia tehtäviä. Yksi suosituista kolmannen osapuolen kirjastoista Javassa regexien käsittelemiseen on `Apache Commons Lang`. Se tarjoaa hyödyllisiä työkaluja kuten `StringUtils`, jotka tekevät joistakin regex-tehtävistä yksinkertaisempia. Tässä on esimerkki, kuinka sitä käytetään alimerkkijonojen esiintymien laskemiseen:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' esiintyy " + count + " kertaa.");
    }
}
```

Käyttääksesi Apache Commons Langia, sinun on sisällytettävä se projektiisi. Jos käytät Mavenia, lisää tämä riippuvuus `pom.xml`-tiedostoosi:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Tarkista viimeisin versio -->
</dependency>
```

Tuloste:
```
'processing' esiintyy 2 kertaa.
```
