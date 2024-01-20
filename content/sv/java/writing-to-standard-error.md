---
title:                "Skriva till standardfel"
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skrivning till standardfelet (stderr) används för att rapportera felmeddelanden. Programmerare gör detta för att skilja vanlig utdata från felrapporter, vilket förenklar felsökning och loggning.

## How to:
Skriv till `System.err` för att få meddelanden till standardfelet.

```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("Detta är vanlig utdata.");
        System.err.println("Detta är ett felmeddelande.");
    }
}
```

Kör kodexemplet. Utdata kommer se ut såhär, men ordningen kan variera beroende på systemets buffring:

```
Detta är vanlig utdata.
Detta är ett felmeddelande.
```

## Deep Dive
Historiskt har felmeddelanden och vanlig utdata separerats för att underlätta behandlingen av dessa strömmar oberoende av varandra. Alternativ för att skriva till stderr inkluderar loggningbibliotek som Log4j, som erbjuder mer sofistikerad hantering av felmeddelanden. Intern användning av `System.err` innebär att Java använder en intern `PrintStream`-instans kopplad till stderr-strömmen som definieras av värdmiljön.

## See Also
- [Oracle's documentation on System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Tutorial on Java logging](https://www.baeldung.com/java-logging-intro)
- [Log4j – A popular Java logging framework](https://logging.apache.org/log4j/2.x/)