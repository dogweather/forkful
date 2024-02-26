---
date: 2024-01-26 00:53:51.771429-07:00
description: "Att hantera fel inneb\xE4r att skriva kod som f\xF6rutser och hanterar\
  \ n\xE4r saker g\xE5r fel. Programmerare g\xF6r det f\xF6r att g\xF6ra programvara\
  \ robust, f\xF6rebygga\u2026"
lastmod: '2024-02-25T18:49:36.089714-07:00'
model: gpt-4-1106-preview
summary: "Att hantera fel inneb\xE4r att skriva kod som f\xF6rutser och hanterar n\xE4\
  r saker g\xE5r fel. Programmerare g\xF6r det f\xF6r att g\xF6ra programvara robust,\
  \ f\xF6rebygga\u2026"
title: Hantering av fel
---

{{< edit_this_page >}}

## Vad & Varför?

Att hantera fel innebär att skriva kod som förutser och hanterar när saker går fel. Programmerare gör det för att göra programvara robust, förebygga krascher och konstigt beteende.

## Hur man gör:

Java använder undantag (exceptions) för att hantera fel. Du omger riskfylld kod med ett `try`-block och fångar undantag med `catch`. Här är ett enkelt exempel:

```java
public class FelhanteringsExempel {
    public static void main(String[] args) {
        try {
            int resultat = dela(10, 0);
            System.out.println("Resultatet är: " + resultat);
        } catch (ArithmeticException e) {
            System.out.println("Hoppsan, kan inte dela med noll!");
        }
    }

    private static int dela(int täljare, int nämnare) {
        return täljare / nämnare;
    }
}
```

Utdata:
```
Hoppsan, kan inte dela med noll!
```

## Djupdykning

Felhantering i Java har utvecklats. Tidiga dagar hade inte undantag; programmerare kontrollerade felkoder. Sedan introducerade Java try-catch-block, vilket tillät mer elegant felhantering.

Alternativ till traditionella `try-catch` inkluderar `try-with-resources` för automatisk stängning av resurser och renare kod, introducerat i Java 7.

Detaljer i implementeringen spelar roll. Till exempel är det vanligtvis dålig praxis att fånga `Exception` eller `Throwable`. Det är för bredt och döljer buggar som du kanske inte är medveten om. Håll dig till specifika undantag.

## Se även

- De officiella Oracle Java-tutorials om undantag: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Java's `try-with-resources`-dokumentation: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java av Joshua Bloch, för bästa praxis gällande undantag.
