---
title:    "Java: Läsning av kommandoradsargument"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Varför

Att kunna läsa in kommandoradsargument är en viktig färdighet för varje Java-programmerare. Genom att kunna läsa in användarens inmatade argument kan man skapa mer dynamiska och anpassningsbara program. Det ger också användaren en mer interaktiv upplevelse. I denna artikel kommer vi att gå igenom hur man läser in kommandoradsargument i Java.

## Hur man gör

Läsningen av kommandoradsargument är enkel i Java. Det första steget är att använda String-arrayen `args` som en parameter i main-metoden. Detta medför att alla inmatade argument blir tillgängliga för programmet. Här är ett exempel på hur du kan skriva ut argumenten i konsolen:

```Java
public static void main(String[] args) { 
    System.out.println("Antal argument: " + args.length); 
    System.out.println("Argument:"); 

    for (String arg : args) { 
        System.out.println(arg); 
    } 
} 
```

Om vi till exempel kör programmet med kommandoradsargumenten `java Test Java is fun`, kommer följande att skrivas ut:

```
Antal argument: 3
Argument:
Java
is
fun
```

Som du ser har alla inmatade argument lagts till i `args`-arrayen. Det finns också möjlighet att använda `Scanner`-klassen för att läsa in användarens inmatning av kommandoradsargument.

## Djupdykning

En viktig aspekt att tänka på när man läser in kommandoradsargument är att kontrollera input för eventuella fel. Till exempel om användaren inte anger tillräckligt många argument kan programmet generera ett undantag. Det är också viktigt att notera att ordningen på argumenten spelar roll. Om du till exempel vill använda en sträng som ett kommando måste den vara innan eventuella flaggor.

Det finns också möjlighet att använda externa bibliotek, som Apache Commons CLI, för att lättare hantera kommandoradsargument med mer komplex funktionalitet.

# Se också

- [Läsning av kommandoradsargument i Java](https://dev.to/lovisaodegard/java-programmering-for-att-lasa-in-kommandoradsargument-57f5)
- [Apache Commons CLI dokumentation](https://commons.apache.org/proper/commons-cli/index.html)