---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulære uttrykk er søkemønstre brukt til å matche tegnstrenger. Vi bruker dem for å forenkle tekstmanipulasjon, validering og for å spare tid.

## How to:
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexEksempel {
    public static void main(String[] args) {
        String tekst = "Hello, dette er en test 123.";
        String mønsterString = "\\b\\d+\\b"; // Matcher heltall

        Pattern mønster = Pattern.compile(mønsterString);
        Matcher matcher = mønster.matcher(tekst);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Output:
```
123
```

## Deep Dive
Regulære uttrykk dukket opp på 1950-tallet, med grep-verktøyet som spredte dem i 1970-årene. Alternativer inkluderer tekstfunksjoner som indexOf eller substring, men disse er mindre fleksible. Implementasjonen av regulære uttrykk i Java skjer gjennom `java.util.regex`-pakken.

## See Also
- Oracle's official regex tutorial: [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Regular-Expressions.info, for en dypere forståelse: [https://www.regular-expressions.info/java.html](https://www.regular-expressions.info/java.html)
- RegExr, for å øve på regexp online: [https://regexr.com/](https://regexr.com/)
