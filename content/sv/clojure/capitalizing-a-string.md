---
title:                "Clojure: Stora bokstäver i en sträng"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till varför man skulle behöva konvertera en sträng till versaler (engelska: capitalize). Det kan till exempel vara för att ge en mer konsistent och enhetlig utseende till en användargränssnitt eller för att sortera en lista av namn på ett korrekt sätt.

## Hur man gör det
Det enklaste sättet att konvertera en sträng till versaler i Clojure är att använda funktionen `capitalize`, som tar emot en sträng som parameter och returnerar strängen med det första tecknet kapitaliserat.

```Clojure
(capitalize "hej på dig") ;; Output: "Hej på dig"
```

Om du behöver konvertera hela strängen till versaler kan du använda funktionen `upper-case`.

```Clojure
(upper-case "hej på dig") ;; Output: "HEJ PÅ DIG"
```

Om du vill kapitalisera varje ord i strängen, kan du använda funktionen `capitalize-words`.

```Clojure
(capitalize-words "hej på dig") ;; Output: "Hej På Dig"
```

Det finns också möjlighet att använda regex för att konvertera strängen till versaler. Till exempel, om du vill att alla bokstäver ska vara versaler förutom den första bokstaven, kan du använda en funktion som denna:

```Clojure
(defn capitalize-first-letter [s] 
  (str (Character/toUpperCase (first s)) (subs s 1)))

(capitalize-first-letter "hej på dig") ;; Output: "Hej på dig"
```

## Djupdykning
När man konverterar en sträng till versaler, är det viktigt att vara medveten om att detta kan påverka specialtecken och diakritiska tecken på ett oväntat sätt. Till exempel, när man använder `upper-case` på en svensk sträng med å, ä eller ö, kommer dessa tecken att försvinna helt. Detta beror på att dessa tecken inte finns i ASCII-tabellen och därför inte omfattas av standardfunktionerna för att konvertera strängar till versaler.

För att undvika denna potentiella problem, kan du använda funktionen `clojure.string/upper-case` istället för `upper-case`. Denna funktion använder Unicode-standard för att hantera specialtecken och diakritiska tecken korrekt.

## Se även
- [Clojure Dokumentation](https://clojure.org/api/cheatsheet)
- [Java Character Klass](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html)
- [Clojure String Library](https://clojuredocs.org/clojure.string)