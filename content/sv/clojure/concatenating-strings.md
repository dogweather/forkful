---
title:    "Clojure: Sammanslagning av strängar"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att konkatenera strängar, eller sammanslå flera strängar till en enda, är en vanlig operation inom programmering. Det kan vara användbart för att skapa en längre sträng från flera kortare delar eller för att formatera utdata på ett snyggt sätt. I denna artikel ska vi gå igenom hur man kan konkatenera strängar i Clojure.

## Så här gör du

För att konkatenera strängar i Clojure, kan du använda funktionen `str`. Den tar emot en eller flera strängar som argument och returnerar en ny sträng som är en sammanslagning av alla argument.

```Clojure
(str "Hej " "på " "dig") ;; Output: Hej på dig
(str "Jag " "är " "en " "Clojure " "utvecklare") ;; Output: Jag är en Clojure utvecklare
```

Du kan också använda operatorn `+`, som i vanlig matematik, för att konkatenera strängar.

```Clojure
(+ "Hello " "world") ;; Output: Hello world
```

Om du vill konkatenera en sträng med en annan typ av data, till exempel ett tal, kan du använda `str` med funktionsanropet `str` inuti.

```Clojure
(str "Jag är " (str 27) " år gammal") ;; Output: Jag är 27 år gammal
```

## Djupdykning

När du använder funktionen `str`, skapas en ny sträng varje gång som funktionen anropas. Detta kan bli ineffektivt om du behöver konkatenera många strängar i en loop eller i en stor applikation. I dessa fall är det bättre att använda `strs`, som tar emot en sekvens och returnerar en sträng som är en konkatenering av alla element i sekvensen.

```Clojure
(strs ["Jag " "är " "en " "Clojure " "utvecklare"]) ;; Output: Jag är en Clojure utvecklare
```

## Se även

- [Clojure dokumentation om str](https://clojuredocs.org/clojure.core/str)
- [Clojure dokumentation om strs](https://clojuredocs.org/clojure.core/strs)
- [En guide till Clojure för nybörjare](https://www.vivekanandarajkumar.com/beginners/2019/02/12/clojure-beginners-guide.html)