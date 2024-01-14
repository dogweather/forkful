---
title:                "Java: Konvertera en sträng till små bokstäver"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Många gånger har du förmodligen stött på en situation där du behöver omvandla en sträng till gemener (lower case) i ditt Java-program. Det kan vara för att jämföra två strängar på ett korrekt sätt eller för att få en enhetlig formatering. Oavsett anledning, är det viktigt att veta hur man går tillväga för att utföra denna uppgift på ett effektivt sätt.

## Hur man gör det

För att omvandla en sträng till gemener i Java, finns det ett inbyggt kommando som kallas "toLowerCase()". Detta kommando ändrar alla stora bokstäver till små bokstäver i strängen. Här är ett exempel på hur man använder detta kommando:

```Java
String str = "HeJ!";
System.out.println(str.toLowerCase());
```

Output: "hej!"

Kommandot "toLowerCase()" kan också användas tillsammans med andra strängmanipulationsmetoder för att utföra mer komplexa uppgifter. Till exempel kan man använda "toLowerCase()" tillsammans med "substring()" för att omvandla en del av en sträng till gemener.

## Djupdykning

När man använder "toLowerCase()" förbättras inte bara strängens formatering, utan det gör också jämförelser mer korrekt och effektivare. Detta beror på att Java interpreterar stora och små bokstäver olika och om man inte är konsekvent kan det leda till felaktiga resultat. Genom att omvandla alla bokstäver till gemener, elimineras risken för felaktiga jämförelser.

Det är också viktigt att notera att "toLowerCase()" endast fungerar med engelska bokstäver. För att hantera icke-engelska bokstäver, måste man använda andra metoder som "toLowerCase(Locale.default)". Man kan också använda "toUpperCase()" för att omvandla en sträng till versaler (uppefall).

## Se även

- Java Strängar (Strings) Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Java Character Klass Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html
- Java Locale Klass Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html