---
title:    "Java: Utskrift av felsökningsutdata"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Varför

Det finns många gånger inom programmering när vi behöver få en bättre förståelse av vad som händer i vårt program. Det kan vara för att hitta buggar eller för att förstå varför vårt program inte fungerar som det ska. Ett sätt att göra det är genom att skriva ut så kallad debug output i vårt program. Detta är en enkel men effektiv metod för att felsöka och förstå koden bättre.

# Så här gör man

För att skriva ut debug output i Java, används metoden `System.out.println()`. Detta kommer att skriva ut den angivna informationen i konsolen. Låt oss ta en titt på ett enkelt exempel:

```Java
String name = "Johan";
int age = 30;
System.out.println("Namn: " + name);
System.out.println("Ålder: " + age);
```

Resultatet av detta skulle vara följande:

```
Namn: Johan
Ålder: 30
```

På detta enkla sätt kan vi visa information som vi kan använda för att felsöka vårt program. Det är också möjligt att skriva ut information om variabler inuti en kodsträng genom att använda sig av placeholders, som `String.format()`-metoden. Exempel:

```Java
String name = "Johan";
int age = 30;
System.out.println(String.format("Jag heter %s och är %d år gammal.", name, age));
```

Detta skulle producera samma resultat som det första exemplet.

# Djupdykning

Att använda debug output är en utmärkt metod för att förstå vad som händer i vårt program. Genom att skriva ut information om variabler och körningsnivåer kan vi enkelt spåra buggar eller identifiera felaktiga uppgifter. Detta är särskilt användbart vid att felsöka mer komplexa problem eller när vi arbetar med flera utvecklare på samma kod.

Det finns också möjlighet att styra utskriften av debug information genom att använda en loggningsramverk, som till exempel Log4j. Genom att använda sådana verktyg kan vi kontrollera när och var våra debug-utskrifter visas och även spara dem till en fil för senare användning.

# Se även

Även om debug output är en användbar metod så är det inte alltid den bästa lösningen för felsökning. Det är alltid bra att vara medveten om andra alternativ, som användning av en debugger eller att använda loggningsramverk. Här är några länkar där du kan läsa mer om dessa:

- [En enkel guide till felsökning med en debugger](https://www.journaldev.com/13639/debugging-java-applications-with-eclipse)
- [En kort introduktion till Log4j](https://www.loggly.com/ultimate-guide/introduction-to-log4j/)

Genom att känna till olika metoder för felsökning kan vi välja den som passar bäst för vårt specifika problem och på så sätt öka vår effektivitet som utvecklare.