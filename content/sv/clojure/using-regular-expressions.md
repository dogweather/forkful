---
title:    "Clojure: Att använda reguljära uttryck"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regex, eller reguljära uttryck, är ett verktyg som hjälper till att söka och manipulera textsträngar. Det är särskilt användbart för att hitta och ersätta specifika mönster i stora mängder data. Genom att använda regex kan du effektivt filtrera och bearbeta text som passar dina behov.

## Hur man använder regex i Clojure

För att använda reguljära uttryck i Clojure måste du importera biblioteket ```clojure.string```. Sedan kan du använda funktionen ```re-find``` eller ```re-seq``` för att hitta matchande mönster i en textsträng.

Exempelvis kan du använda följande kod för att hitta alla förekomster av ordet "hund" i en textsträng:
```Clojure
(require '[clojure.string :as str])

(def text "Jag älskar hundar, speciellt golden retrievers.")

(str/re-seq #"hund" text)
```

Detta skulle producera en lista med en matchning för ordet "hund" i textsträngen.

Du kan också använda regex för att ersätta delar av en textsträng med ett annat mönster. Detta kan göras med hjälp av funktionen ```re-pattern``` och ```str/replace-first``` eller ```str/replace``` beroende på om du vill ersätta den första matchningen eller alla matchningar.

## Djupdykning

Det finns många olika metakaraktärer och specialtecken som du kan använda i reguljära uttryck för att uttrycka komplexa mönster. Till exempel kan du använda ```*``` för att matcha noll eller flera förekomster av en karaktär, ```+``` för att matcha en eller flera förekomster, och ```^``` för att matcha ett mönster i början av en textsträng.

Du kan också använda grupper med parenteser för att extrahera specifika delar av en matchning, och sedan använda dessa delar i ditt utbytet mönster.

För att bli mer bekant med olika regex-mönster och deras användningsområden rekommenderas det att öva och experimentera med det.

## Se även

- [Clojure.org - Regular Expressions](https://clojure.org/guides/regular_expressions)
- [RegExr - Regex-tutorials och tester](https://regexr.com/)