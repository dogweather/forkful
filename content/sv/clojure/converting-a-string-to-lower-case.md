---
title:                "Clojure: Omvandla en sträng till gemener"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är ett vanligt behov inom programmering. Det kan vara användbart när man vill jämföra strängar eller när man vill hantera inmatade användardata på ett enhetligt sätt. I denna bloggpost kommer vi att utforska hur man gör detta på ett enkelt sätt med hjälp av Clojure.

## Så här gör du

För att konvertera en sträng till gemener i Clojure, kan du använda funktionen "lower-case". Här är ett enkelt exempel som visar hur man kan använda den:

```Clojure
(lower-case "Hej, Världen!")
```

Output: "hej, världen!"

Som du kan se i exemplet, omvandlas alla versaler i strängen till gemener. Detta fungerar inte bara för engelska tecken, utan också för andra språk som använder Unicode-tecken.

Om du vill konvertera en sträng som finns lagrad i en variabel, kan du använda följande kod:

```Clojure
(def my-sträng "Välkommen Till Clojure!")

(lower-case my-sträng)
```

Output: "välkommen till clojure!"

## Och så till det djupa

Om vi tittar under huven, så är "lower-case" funktionen inte bara en enkel metod för att byta bokstavsstadier. Istället använder den funktionen "clojure.string/lower-case" som utnyttjar Java's klass "java.lang.String". Denna klass tillhandahåller metoden "toLowerCase()" som utför den faktiska konverteringen.

Men det finns mer att upptäcka! För att bli mer flexibel i hur du vill konvertera till gemener, kan du också utforska "clojure.string/case" funktionen. Denna funktion tillåter dig att ange olika språkkoder och specialtecken för mer precisa konverteringar.

Nu när du har en större förståelse för hur konvertering till gemener fungerar, kan du börja implementera det i dina projekt och förbättra användarupplevelsen.

## Se även

- Clojure dokumentation för "lower-case": https://clojuredocs.org/clojure.string/lower-case
- Java dokumentation för "toLowerCase()": https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
- Clojure dokumentation för "case": https://clojuredocs.org/clojure.string/case

Med dessa resurser kan du fortsätta att utforska och lära dig mer om konvertering av strängar till gemener i Clojure. Lycka till!