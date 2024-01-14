---
title:                "Java: Läsning av kommandoradsargument"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en viktig del av Java-programmering, eftersom det ger användarna möjlighet att ge direktiv till programmet vid körning. Det är en bra funktion som ökar programmens flexibilitet och ger en mer anpassningsbar användarupplevelse.

## Så här gör du

Att läsa kommandoradsargument i Java kan göras med hjälp av några enkla kodrader. Först måste du använda "args" som en parameter i huvudmetoden, som visas nedan:

```Java
public static void main(String[] args) {

}
```

Därefter kan du använda en loop för att läsa in varje argument som skickas med vid körning:

```Java
for (int i = 0; i < args.length; i++) {
    // Läs in varje argument
    String argument = args[i];
    
    // Gör något med argumentet (ex. skriv ut det)
    System.out.println(argument);
}
```

När du skriver ut argumenten kommer du att se dem i samma ordning som de skickades in vid körning. Du kan också använda en enkel if-sats för att kolla om ett visst argument har skickats med, till exempel:

```Java
if (argument.equals("-help")) {
    // Visa hjälpmeddelande
    System.out.println("Det här är ett hjälpmeddelande.");
}
```

Dessa enkla kodexempel visar hur lätt det är att läsa in och använda kommandoradsargument i Java.

## Djupdykning

För dem som vill ha en djupare förståelse för hur kommandoradsargument fungerar i Java, finns det flera aspekter att överväga. Till exempel är det viktigt att veta att argumenten behandlas som en strängvektor, där varje argument representeras som en egen sträng. Detta innebär att det inte finns några speciella datastrukturer för kommandoradsargument i Java.

En annan viktig punkt att notera är att det inte finns någon standard för hur argumenten ska skickas in vid körning. Det kan skilja sig åt beroende på vilken kompilator som används eller vilket operativsystem som programmet körs på.

Det är också värt att nämna att läsa in kommandoradsargument kan vara användbart för att göra enkla konfigureringar av ett program. Till exempel kan ett argument användas för att välja vilken fil som ska läsas in eller vilket alternativ som ska aktiveras i programmet.

## Se även

- [Java Dokumentation: Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java Tutorial: Reading Command Line Arguments](https://www.javatpoint.com/command-line-argument-in-java)