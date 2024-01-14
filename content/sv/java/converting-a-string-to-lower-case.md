---
title:    "Java: Omvandla en sträng till gemener"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift inom Java-programmering. Detta kan vara användbart för att förbereda data inför jämförelser eller för att förbättra läsbarheten av en sträng.

## Så här gör du

För att konvertera en sträng till små bokstäver, kan vi använda metoden `toLowerCase()` i `String` klassen. Detta är en inbyggd metod som konverterar alla stora bokstäver i en sträng till små bokstäver. Här är en enkel kodexempel:

```java
String input = "HEJ!"; 
String output = input.toLowerCase(); 
System.out.println(output); 
```

Outputen av denna kod skulle bli: "hej!".

För att konvertera en sträng till små bokstäver utan att använda `toLowerCase()`-metoden kan vi också använda oss av ASCII-värden för varje bokstav i strängen. Om du är intresserad av denna metod, kan du söka efter "ASCII-tabell" för en fullständig lista över de olika värdena för bokstäverna.

## Djupdykning

De som är nya inom Java-programmering kan undra vilka typer av variationer som finns mellan små och stora bokstäver. Detta kan vara särskilt viktigt om man arbetar med jämförelser eller sorteringsfunktioner.

Ett vanligt missförstånd är att endast engelska bokstäver påverkas av konvertering till små bokstäver, men faktum är att detta också gäller för andra språk som använder sig av specialtecken. Till exempel kommer den svenska bokstaven Å att omvandlas till å när man använder `toLowerCase()`-metoden.

Det är också viktigt att notera att när vi konverterar en sträng till små bokstäver, så påverkas inte sifforna eller andra tecken. Så om vi exempelvis konverterar "Hej2!" till små bokstäver, kommer det att fortfarande vara "hej2!".

Det finns också andra användningsområden för att konvertera en sträng till små bokstäver, såsom inför indexering och sökning av data. Om man inte konverterar alla bokstäver till små bokstäver, kan det leda till problem med sökningar som inte matchar på grund av en blandning av små och stora bokstäver.

## Se även

- [Java String Klass Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [ASCII-tabell](https://ascii.cl/)
- [Comparison of ASCII and Unicode](https://en.wikipedia.org/wiki/Comparison_of_ASCII_and_Unicode)