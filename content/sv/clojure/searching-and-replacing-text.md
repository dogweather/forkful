---
title:                "Söka och ersätta text"
html_title:           "Clojure: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Varför

Att söka och byta ut text är en vanlig uppgift för programmerare, oavsett vilket språk de arbetar med. Det kan vara ett viktigt steg i att lösa ett problem eller för att uppdatera kod som har blivit föråldrad. Med Clojure är det lätt att söka och ersätta text på ett effektivt sätt.

##Så här gör du

Du kan enkelt söka och byta ut text i Clojure genom att använda den inbyggda "replace" funktionen. Här är ett enkelt exempel på hur du kan använda den:

```Clojure
(replace "hej" "hello" "Hej världen!") 
```

Detta kommer att söka igenom textsträngen "Hej världen!" och byta ut alla förekomster av "hej" med "hello". Detta ger dig följande output:

```Clojure
"Hello världen!"
```

För att söka igenom en hel fil och ersätta text kan du använda funktionen "slurp" för att läsa in filen som en textsträng och sedan använda "replace" på den. Till exempel:

```Clojure
(with-open [file (clojure.java.io/reader "minfil.txt")] 
  (let [content (slurp file)] 
    (replace "hund" "katt" content)))
```

Detta kommer att söka igenom filen "minfil.txt" och byta ut alla förekomster av "hund" med "katt". Outputen är den uppdaterade texten från filen.

##Djupdykning

Det finns också andra alternativ för att utföra sökningar och byta ut text i Clojure, såsom att använda reguljära uttryck med funktionen "re-seq" eller att använda molnfunktioner som "sed". Det är viktigt att notera att Clojure används för att manipulera data på en funktionell nivå, vilket gör att sökningar och ersättningar kan bli enklare och mer lättlästa jämfört med andra språk.

##Se även

- [ClojureDocs](https://clojuredocs.org/clojure.core/replace)
- [En introduktion till Clojure](https://github.com/hjorturlarsen/artikel-clojure)
- [Tutorial: Att bygga en enkel webbapplikation med Clojure och Ring](https://www.dailydrip.com/blog/clojure-building-web-apps-with-compojure)