---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text i kod innebär att vi identifierar specifika strängar i en större textmassa och byter ut dem mot något annat. Programmerare gör detta regelbundet för att snabbt ändra variabelnamn, korrigera felstavningar eller modifiera kodens funktionalitet.

## Så här gör du:
Java ger oss metoden `replace()` tillgänglig i `String` klassen för att utföra sökning och ersättning. Här är ett exempel:

```Java
public class Main {
   public static void main(String[] args) {
      String str = "Hej, jag älskar Java programmering!";
      String replacedStr = str.replace("Java", "Pyton");
      System.out.println(replacedStr);
   }
}
```
När du kör koden kommer utdata att vara:
```
"Hej, jag älskar Pyton programmering!"
```
I det här exemplet har vi bytt ut ordet "Java" med "Pyton" i strängen.

## Djupare Dyk:
Söka och ersätta text har historiskt sett varit en viktig del av programmering sedan dess begynnelse. Från Unix's `sed` kommando till moderna infödda metoder i de flesta programmeringsspråk, är det mänsklig önskan att automatisera repetitiva uppgifter.

I Java, kan du också använda `replaceAll()` metoden som tar ett reguljärt uttryck som det första argumentet. Det ger fler alternativ för sökningen, men kan vara överdriven för enkla användningsfall.

Om vi ser till implementationen, använder `replace()` metoden intern i Java en loop för att iterative över varje karaktär i strängen och gör substitutionen om matchning hittas.

## Se även:
Fler resurser om detta ämne finns på:

- `String` klassdokumentation: [https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- Java Tutorials – Regular Expressions: [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- "Söka och ersätta med Regular Expressions" – Stack Overflow: [https://stackoverflow.com/questions/16510/how-do-i-do-a-find-and-replace-in-java-using-regular-expressions](https://stackoverflow.com/questions/16510/how-do-i-do-a-find-and-replace-in-java-using-regular-expressions)