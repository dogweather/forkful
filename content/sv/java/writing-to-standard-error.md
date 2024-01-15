---
title:                "Skriv till standardfel"
html_title:           "Java: Skriv till standardfel"
simple_title:         "Skriv till standardfel"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Ibland när vi kör våra Java-program, kan det hända att något går fel. Istället för att programmet bara fortsätter köra, vill vi veta vad som gick fel och varför. Här kommer skrivning till standard error in i bilden.

## Hur man gör det

Skrivning till standard error i Java är enkelt. Allt du behöver göra är att använda ```System.err.println()``` istället för ```System.out.println()```. Här är ett exempel:

```Java
public static void main(String[] args) {
    System.err.println("Detta är ett felmeddelande.");
}
```

Detta kommer att skriva ut det meddelandet till standard error och om något går fel i ditt program, så kommer du att kunna se det.

## Djupdykning

Så vad är egentligen standard error och varför skriver vi till det istället för att bara använda ```System.out```? I Java finns det tre standardutströmningar: ```System.in```, ```System.out``` och ```System.err```. ```System.out``` används för vanligt utdata, medan ```System.err``` används för felutdata. Det är en bra idé att skilja på dessa två för att kunna felsöka enklare och bättre förstå ditt programs beteende.

## Se även

- [Java API](https://docs.oracle.com/javase/8/docs/api/)
- [Skrivning till standard out i Java](https://www.java67.com/2015/08/how-to-print-to-standard-error-in-java.html)
- [Felsökningsguide för Java](https://www.baeldung.com/java-debugging)