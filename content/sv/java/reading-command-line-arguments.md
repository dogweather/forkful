---
title:                "Java: Läsa kommandoradargument"
simple_title:         "Läsa kommandoradargument"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig färdighet för programmerare, särskilt för dem som arbetar med Java. Detta gör det möjligt för oss att interagera med programmet och använda det på olika sätt beroende på vilken input som ges. Låt oss titta på hur man läser kommandoradsargument i Java.

## Så här gör du

Först och främst måste vi förstå vad kommandoradsargument är. Det är en eller flera strängar som ges till programmet som argument när det körs från kommandoraden. Dessa argument kan användas av programmet för att bestämma dess beteende eller för att ge användaren möjlighet att påverka det.

För att läsa kommandoradsargument i Java använder vi main-metoden. Detta är den metod som körs när vi startar ett Java-program från kommandoraden. Här är en enkel kod för att läsa in ett kommandoradsargument och skriva ut det:

```Java
public static void main(String[] args){
    System.out.println("Det angivna argumentet är: " + args[0]);
}
```

Låt oss säga att vi sparar denna kod som "CommandLineArgs.java" och kör den från kommandoraden med argumentet "hello". Vi skulle få följande output:

```
Det angivna argumentet är: hello
```

Vi kan också läsa in flera kommandoradsargument och använda dem i vår kod på olika sätt. Till exempel kan vi använda dem som nummer och utföra olika beräkningar med hjälp av dem. Här är ett annat exempel på hur man kan läsa och använda flera kommandoradsargument:

```Java
public static void main(String[] args){
    int a = Integer.parseInt(args[0]);
    int b = Integer.parseInt(args[1]);
    System.out.println("Summan av argumenten är: " + (a + b));
}
```

Om vi kör detta program med argumenten "5" och "10", skulle vi få följande output:

```
Summan av argumenten är: 15
```

Det finns många olika sätt att använda kommandoradsargument på i våra Java-program och det är upp till oss som programmerare att vara kreativa och hitta nya sätt att använda dem på.

## Djupdykning

Det finns flera saker att tänka på när man läser kommandoradsargument i Java. Till exempel är indexet för det första argumentet alltid 0 och det sista argumentet är alltid "args.length - 1". Detta är viktigt att komma ihåg för att undvika eventuella fel i vår kod.

Det är också viktigt att hantera felaktig input från kommandoraden. Om vi till exempel förväntar oss ett nummer som argument men får en sträng, kan vårt program krascha. Vi måste därför använda felhantering för att säkerställa att vårt program fungerar korrekt oavsett vilken input som ges.

En annan intressant aspekt av kommandoradsargument är att de kan vara mycket användbara för att automatisera uppgifter. Till exempel kan vi använda dem för att skapa ett program som söker igenom filer eller mappar baserat på olika kriterier som ges som argument. Detta sparar oss mycket tid och arbete jämfört med att manuellt söka efter filer.

## Se även

- [Java Documentation - Command Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [JournalDev - Java Command Line Arguments](https://www.journaldev.com/13551/java-command-line-arguments)
- [Baeldung - Java Command Line Arguments](https://www.baeldung.com/java-command-line-arguments)