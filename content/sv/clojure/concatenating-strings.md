---
title:                "Sammanslagning av strängar"
html_title:           "Clojure: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Concatenation av strängar handlar om att slå samman två eller flera textsträngar till en enda sträng. Programmers gör det ofta för att skapa dynamiska meddelanden eller för att bygga mer komplexa strukturer såsom HTML-dokument.

## Så här gör du:
Det finns flera sätt att utföra strängkonkatenation i Clojure. Det vanligaste sättet är att använda `str` funktionen, vilket låter dig konkatenare alla typer av värden, inklusive strängar.

```Clojure
(str "Hej " "världen!") 
;; => "Hej världen!"
```

Om du vill konkaternera flera strängar i en lista, kan du använda `str/join` funktionen.

```Clojure
(str/join ", " ["apple" "banana" "orange"]) 
;; => "apple, banana, orange"
```

Du kan också använda `format` funktionen för att få mer kontroll över utmatningen. Detta låter dig till exempel formatera nummer och lägga till variabler i din utmatning.

```Clojure
(format "Idag är det %d grader ute och %s." 26 "soligt")
;; => "Idag är det 26 grader ute och soligt."
```

## Djupdykning:
Strängkonkatenering är en vanlig operation i många programmeringsspråk och har funnits sedan tidiga datorer. Alternativ till `str` och `format` funktionerna är `StringBuilder` som används i Java och `concat` funktionen i Clojure.

Implementeringen av `str` i Clojure använder `StringBuilder` under huven för att vara så effektiv som möjligt. Den kan också hantera många olika typer av värden, inklusive nil, utan att krascha.

## Se även:
Officiell dokumentation för strängkonkatenation i Clojure: https://clojuredocs.org/clojure.core/str