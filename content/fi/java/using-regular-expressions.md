---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulaarilausekkeet (regular expressions) on hakemisto työkaluja tekstisyötteiden käsittelyyn. Ne auttavat löytämään, erottamaan, ja muuttamaan tekstiä tehokkaasti. Koodarit käyttävät niitä, koska ne säästävät aikaa ja tekevät monimutkaiset tekstihaasteet yksinkertaisiksi.

## How to:
### Tekstin etsiminen
Etsitään kaikki luvut annetusta tekstistä.
```java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Tervetuloa ohjelmoimaan vuonna 2023!";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Tulostus:
```
2023
```

### Tekstin korvaaminen
Korvataan kaikki välimerkit tyhjällä merkkijonolla.
```java
public class RegexReplace {
    public static void main(String[] args) {
        String text = "Hei, miten menee? Oletko ohjelmoija!";
        String modifiedText = text.replaceAll("\\p{Punct}", "");
        System.out.println(modifiedText);
    }
}
```
Tulostus:
```
Hei miten menee Oletko ohjelmoija
```

## Deep Dive
Regulaarilausekkeet otettiin käyttöön 1950-luvulla ja ovat sittemmin kehittyneet. Java käyttää `java.util.regex` pakettia regexp-tukeen. Vaihtoehtoisia menetelmiä tekstinkäsittelyyn on kuten `String`-luokan metodeja, mutta ne eivät yllä regulaarilausekkeiden joustavuuteen ja voimaan. Suorituskyvyn kannalta on hyvä muistaa, että regulaarilausekkeiden tehokkuus riippuu paljon niiden monimutkaisuudesta ja käytetystä JDK:sta.

## See Also
Java regulaarilausekkeista lisää:
- [Oracle's official Java tutorials on regex](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [regular-expressions.info Tutorial](https://www.regular-expressions.info/tutorial.html)

Haluatko testata regulaarilausekkeita helposti?
- [RegExr](https://regexr.com/) - Sivusto regulaarilausekkeiden testaamiseen ja opetteluun.
- [Regex101](https://regex101.com/) - Regulaarilausekkeiden testaustyökalu ja oppimisympäristö.
