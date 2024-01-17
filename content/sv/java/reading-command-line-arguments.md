---
title:                "Läsa kommandoradsargument"
html_title:           "Java: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av kommandoradsargument är när man läser in information från kommandoraden genom att skriva in kommandoradsargument efter programmets namn. Detta är en viktig del av programmering eftersom det tillåter användaren att ange specifik information eller inställningar för programmet vid körning.

## Så här gör du:
```Java
public class ArgumentExample{
    public static void main(String[] args){
        // Programkod här
        String argument = args[0]; // Läs in kommandoradsargument
        System.out.println(argument); // Skriv ut argumentet
    }
}
```
Programmet ovan tar emot ett kommandoradsargument och skriver ut det till användaren. Om man till exempel kör programmet med kommandoradsargumentet "Hello", så kommer det att skriva ut "Hello" till skärmen.

## Djupdykning:
Att läsa kommandoradsargument har funnits sedan de tidiga dagarna av datoranvändning och är fortfarande ett viktigt koncept inom programmering. Alternativ till att läsa kommandoradsargument inkluderar GUI (Graphical User Interface) där användaren kan ange information genom ett grafiskt gränssnitt, och läsning från filer som lagrar information som sedan kan läsas in av programmet. När man läser kommandoradsargument är det viktigt att hantera felaktig eller inkorrekt formaterad input för att undvika kraschar eller felaktiga resultat i programmet.

## Se även:
Läsa kommandoradsargument är en viktig del av att skapa robusta och användarvänliga program. Här är några källor för mer information och exempel:

- [Oracle Java Documentation - Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Spring Framework - Command Line Arguments](https://www.baeldung.com/spring-boot-command-line-arguments)
- [Codingame - How to Use Command Line Arguments in Java](https://www.codingame.com/playgrounds/4181/how-to-use-command-line-arguments-in-java)