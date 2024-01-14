---
title:                "Clojure: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en dagmånad till en sträng är en viktig del av programmering, särskilt inom datahantering och hantering av användarens inmatningar. Det är också en grundläggande färdighet för att hantera datum och tid i allmänhet.

## Hur man gör det

För att konvertera ett datum till en sträng, kan du använda funktionen `str` följt av datumet och det format du vill ha det i. Till exempel:

```Clojure
(str "Jul 12, 2021" "dd-MM-yyyy")
```

Detta resulterar i strängen "12-07-2021". Som du kan se kan du ange det format som passar bäst för ditt ändamål. Här är några andra exempel på format:

```Clojure
(str "12/7/2021" "MM/dd/yyyy") ; resulterar i "07/12/2021"
(str "12 July, 2021" "dd MMMM, yyyy") ; resulterar i "12 juli, 2021"
(str "7/12/2021" "dd/MMM/yyyy") ; resulterar i "12/jul/2021"
```

Om du vill ha en kortare version av månadens namn kan du använda `String/substring` funktionen som följande exempel:

```Clojure
(str "12 July, 2021" "dd MMM, yyyy") ; resulterar i "12 jul, 2021"
(str "12 July, 2021" "dd MMMM, yyyy") ; resulterar i "12 jul, 2021" 
```

Du kan också använda `String/toUpperCase` för att få månadsnamnet i versaler.

## Djupdykning

För mer avancerade alternativ, kan du använda biblioteket `clj-time`, som ger en rad användbara funktioner för att konvertera datum och tid. Det finns också möjlighet att använda de inbyggda funktionerna `SimpleDateFormat` och `DateTimeFormatter`.

Ett annat konverteringsalternativ är att använda Java-klassen `java.util.Calendar` som ger en mängd olika format och funktioner för att hantera datum och tid.

## Se även

- [str function](https://clojuredocs.org/clojure.core/str)
- [clj-time library](https://github.com/clj-time/clj-time)
- [SimpleDateFormat Java class](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [DateTimeFormatter Java class](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [java.util.Calendar Java class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)