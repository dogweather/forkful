---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Läsning av Kommandoradsargument med Java

## Vad & Varför?

Kommandoradsargument är inputvärden som skickas till ett Java-program vid dess start. Java-programmerare använder dem för att justera programbeteende utan att ändra koden.

## Så här gör du:

I Java fångar du kommandoradsargument med `main()` metoden. Här är ett enkelt exempel:

```Java
public class Main {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println("Argument: " + arg);
        }
    }
}
```

Om du kör ditt program med `java Main FirstArg SecondArg`, så kommer output att vara:

```Java
Argument: FirstArg
Argument: SecondArg
```

## Djupdykning

1. **Historisk kontext**: Läsning av kommandoradsargument är en programmetod som har använts sedan de tidiga dagarna av CLI-baserade operativsystem.

2. **Alternativ**: Om ditt program behöver mer komplexa inmatningar kan du använda bibliotek som JCommander eller Apache CLI.

3. **Implementeringsdetaljer**: Argumenten till `main()` metoden, dvs. `String[] args`, får värdena direkt från operativsystemet. Orden och tecknen du skriver efter `java classname` blir strängar i detta strängarray.

## Se även:

- [Officiell Java-dokumentation om kommandoradsargument](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [JCommander, en Java-bibliotek för tolkning av kommandoradsparametrar](http://jcommander.org/)
- [Apache Commons CLI, ett API för att tolka kommandoradsalternativ](https://commons.apache.org/proper/commons-cli/)

Glöm inte att öva för att bli en effektiv Java-programmerare! God programmering!