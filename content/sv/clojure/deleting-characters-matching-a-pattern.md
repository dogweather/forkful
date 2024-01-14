---
title:    "Clojure: Ta bort tecken som matchar ett mönster"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför du skulle vilja engagera dig i att ta bort tecken som matchar ett mönster i Clojure. Det kan finnas flera anledningar till varför detta kan vara en användbar funktion i ditt program.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i Clojure, kan du använda funktionen "remove" tillsammans med regexp-funktionen "re-matches". Se nedan för ett kodexempel:

```Clojure
(remove #(re-matches #"abc" %) ["a" "b" "abc" "def"])
```

Detta kodexempel tar bort alla tecken som matchar mönstret "abc" från en vektor som innehåller "a", "b", "abc" och "def". Det kommer att producera följande utmatning:

```Clojure
("a" "b" "def")
```

Här kan vi se att tecknet "abc" har tagits bort från vektorn.

## Fördjupning

Funktionen "re-matches" är en del av Clojure's regexp-bibliotek och används för att söka efter mönster i strängar. Det tar två argument: ett mönster och en sträng. Om en matchning hittas, returnerar funktionen en matchdatastruktur som sedan kan användas för att utföra olika åtgärder, till exempel att ta bort matchande tecken. Mer information om regexp-biblioteket och dess funktioner finns tillgängligt i Clojure's dokumentation.

## Se även

- [Clojure regexp-dokumentation](https://clojure.org/guides/learn/regular_functions)
- [Dokumentation för Clojure's "remove" -funktion](https://clojuredocs.org/clojure.core/remove)