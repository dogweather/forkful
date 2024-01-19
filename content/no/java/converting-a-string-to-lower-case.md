---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å konvertere en streng til små bokstaver er å endre alle store bokstaver i en streng til deres mindre ekvivalenter. Programmerere gjør dette som en del av tekstbehandling for å forenkle sammenligning og søk.

## Hvordan:
La oss se på et eksempel i Java for å konvertere en streng til små bokstaver.

```Java
public class Main {

    public static void main(String[] args) {
    
        String myStr = "HELLO, WORLD!";
        String lowerCaseStr = myStr.toLowerCase();
        
        System.out.println(lowerCaseStr);
        
    }

}
```

Når vi kjører denne kodesnippet, er utdata:

```
hello, world!
```

## Dyp Dykk:
Historisk kontekst: Metoden `toLowerCase()` har vært tilgjengelig i Java siden det første frigjøringen, Java 1.0.

Alternativer: Du kan også konvertere en streng til små bokstaver ved bruk av Apache's Commons `StringUtils.lowerCase()` metode. 

Detaljer om implementasjon: `toLowerCase()` metoden i Java bruker standardregler for Unicode for å konvertere. Det kan også ta en `Locale` som en parameter for å gi spesifikke lokaliserte regler for konvertering.

## Se Også:
1. [Java String toLowerCase() Method](https://docs.oracle.com/javase/tutorial/i18n/text/convertToLower.html)
2. [Apache Commons StringUtils.lowerCase() Method](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html#lowerCase-java.lang.String-)
3. [Unicode Case Folding](https://unicode.org/faq/casemap_charprop.html)