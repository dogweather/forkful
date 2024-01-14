---
title:    "Clojure: Användning av reguljära uttryck"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck, eller regular expressions på engelska, är ett kraftfullt verktyg inom Clojure-programmering. Med hjälp av reguljära uttryck kan du söka, ersätta och manipulera text på ett effektivt sätt. Det är ett oumbärligt verktyg när du arbetar med stora datamängder eller behöver göra avancerade sökningar i textfiler. Läs vidare för att lära dig hur du kan använda reguljära uttryck i dina Clojure-projekt.

## Så här

För att använda reguljära uttryck i Clojure behöver du importera biblioteket "java.util.regex". Sedan kan du använda funktionen "re-matches" för att hitta matchande delar i en sträng. Låt oss säga att du vill hitta alla ord som börjar med bokstaven "a" i en text. Du kan använda följande kod:

```Clojure
(import [java.util.regex Pattern])
(def pattern (Pattern/compile "^a\\w+"))
(re-matches pattern "apple is a fruit") ; => ["apple"]
```

För att göra en mer avancerad sökning kan du använda olika symboler för att specificera vad du letar efter. Till exempel kan du använda "\" för att söka efter en särskild karaktär, "+" för att hitta ett eller flera tillfällen av en karaktär, och "*" för att hitta noll eller flera tillfällen av en karaktär. Här är ett exempel på hur du kan hitta alla ord som innehåller bokstaven "a":

```Clojure
(import [java.util.regex Pattern])
(def pattern (Pattern/compile "\\w*a\\w*"))
(re-matches pattern "apple is a fruit") ; => ["apple", "fruit"]
```

## Djupdykning

Det finns många olika symboler och mönster som du kan använda i reguljära uttryck för att fånga specifika delar av en sträng. Du kan till exempel använda parenteser för att gruppera olika delar av ett uttryck och sedan extrahera dem med hjälp av "re-groups" funktionen. Det är också möjligt att använda "re-matches" för att hitta flera matchningar i en sträng och "re-find" för att hitta den första matchningen.

Det är viktigt att komma ihåg att reguljära uttryck är väldigt kraftfulla men också komplexa. Det kan ta lite tid att lära sig dem, men när du väl behärskar dem kan de spara dig mycket tid och arbete.

## Se även

Här är några användbara resurser för att lära dig mer om reguljära uttryck och hur du kan använda dem i dina Clojure-projekt:

- [Clojure Regular Expressions Documentation](https://clojure.org/reference/regular_expressions)
- [RegExr](https://regexr.com/) - ett verktyg för att testa reguljära uttryck online
- [Learn Reagent Expressions](https://learn.reagentproject.org/docs/widgets/regex) - en interaktiv guide för att lära sig reguljära uttryck

Lycka till med att använda reguljära uttryck i dina Clojure-projekt!