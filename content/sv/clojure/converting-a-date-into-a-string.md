---
title:    "Clojure: Omvandla ett datum till en sträng"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara en viktig uppgift för många utvecklare. Det kan hjälpa till att visualisera och kommunicera data på ett mer läsbart sätt eller som input till andra funktioner.

## Så här gör du

För att konvertera ett datum till en sträng i Clojure, använd funktionen `format`. Se nedan för ett exempel på hur man skulle konvertera dagens datum till en sträng med formatet "DD/MM/YYYY".

```Clojure
(format "%1$td/%1$tm/%1$tY" (java.util.Date.))
```

Det här skulle skapa en sträng som ser ut som "24/05/2021" beroende på vilket datum det är när du kör koden. Funktionen `format` tar två argument: första argumentet är ett formateringssträng som bestämmer hur datumen kommer att presenteras, och det andra argumentet är det faktiska datumet som ska konverteras.

Det finns många olika sätt att formatera datumsträngar, så se till att ta en titt på dokumentationen för fler exempel och möjligheter.

## Djupdykning

När du använder funktionen `format` är det viktigt att förstå vad varje symbol i formateringssträngen betyder. Till exempel står "d" för dag, "m" för månad och "Y" för år. Genom att kombinera dessa symboler på rätt sätt kan du skapa en helt anpassad datumsträng.

Det är också värt att notera att funktionen `format` tar emot ett Java-datumobjekt som argument. Om du behöver arbeta med ett annat datumformat i ditt program måste du konvertera det till ett Java-datum först innan du kan använda `format`.

## Se även

- Dokumentation för `format` funktionen: https://clojuredocs.org/clojure.core/format
- Guide för datum och tid i Clojure: https://clojure.org/guides/date_and_time