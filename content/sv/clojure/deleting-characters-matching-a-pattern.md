---
title:                "Clojure: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart för att rensa upp data eller filtrera bort oönskade tecken i en sträng.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster använder man en kombination av funktionerna `str` och `re-seq`.

```Clojure
(str (re-seq #"[*]" "Det här är en *provsats* med *tecken* att ta bort."))
```

Detta kommer att skapa en ny sträng utan några asterisker (`*`).

Output: "Det här är en provsats med tecken att ta bort."

## Djupdykning

För att förstå hur detta fungerar djupare, låt oss bryta ner koden:

- `re-seq` används för att matcha regelbundna uttryck i en sträng.
- `#"[*]"` är det mönster som vi vill matcha, i detta fall en asterisk.
- `"Det här är en *provsats* med *tecken* att ta bort."` är den sträng som vi vill filtrera.
- `str` används för att kombinera resultaten från `re-seq` till en enda sträng.

Genom att använda `re-seq` för att matcha ett visst mönster och sedan använda `str` för att kombinera resultatet kan vi enkelt ta bort oönskade tecken från en sträng.

## Se också

- [Clojure Dokumentation](https://clojure.org/)
- [RegEx Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Clojure Cookbook](https://www.clojure-cookbook.com/)