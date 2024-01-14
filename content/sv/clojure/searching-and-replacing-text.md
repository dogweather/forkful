---
title:    "Clojure: Sökning och ersättning av text"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering, oavsett språk. Det kan hjälpa till att automatisera processer och effektivisera arbetsflöden. Med hjälp av Clojure kan man enkelt söka och ersätta text med hjälp av inbyggda funktioner och regex-uttryck. Det är användbart för att manipulera data, göra stora ändringar i en fil eller för att göra batch-uppdateringar.

## Hur man gör

För att söka och ersätta text i Clojure kan man använda sig av `replace`-funktionen tillsammans med ett regex-uttryck. Detta gör att man kan söka efter ett mönster i en sträng och ersätta det med en annan sträng.

Här är ett exempel på hur man kan använda `replace` för att byta ut alla förekomster av ordet "hej" till "tjena" i en sträng:

```Clojure
(replace #"hej" "tjena" "Hej, hur mår du?") ; => "Tjena, hur mår du?"
```

Om man behöver söka efter ett mer komplext mönster kan man använda sig av `re-find`-funktionen tillsammans med ett regex-uttryck för att hitta den första matchningen och sedan använda `replace` på den.

```Clojure
(replace #"hel|god" "bra" (re-find #"h[eu]l|god" "Hej alla!" )) ; => "Bra alla!"
```

Man kan även använda sig av `replace` för att ersätta flera förekomster av ett mönster med en annan sträng. Detta kan man göra genom att använda sig av en `hash-map` där nycklarna representerar de sökta mönstren och värdena är de ersättande strängarna.

```Clojure
(replace {"hej" "tjena" "hallå" "tja"} "Hej, hallå!") ; => "Tjena, tja!"
```

## Djupdykning

I Clojure används regex-uttryck genom att placera dem mellan `#"` och `"` vilket gör att de tolkas som `java.util.regex`-objekt. Det finns många olika metakaraktärer och syntax som kan användas i regex-uttryck för att skapa mer avancerade mönster. En bra resurs för att lära sig mer om regex är [Regular-Expressions.info](https://www.regular-expressions.info/clojure.html).

När man använder `replace` i Clojure är det också viktigt att komma ihåg att den returnerar en kopia av strängen och inte manipulerar den ursprungliga. Om man vill ersätta förekomster i en specifik del av en sträng kan man använda sig av den inbyggda `replace-first`-funktionen.

## Se även

- [Clojure regex-kodreferens](https://clojure.org/reference/regular_expressions)
- [Learning Clojure: Part 4 - Regex](https://pretzl