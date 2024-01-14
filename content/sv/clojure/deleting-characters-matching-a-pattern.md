---
title:                "Clojure: Radera tecken som matchar ett mönster"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart när man vill rensa data eller förbereda det för vidare bearbetning.

## Hur man gör

För att ta bort tecken som matchar ett mönster i Clojure använder man sig av funktionen `str/replace` tillsammans med reguljära uttryck. Låt oss säga att vi har en sträng som innehåller både bokstäver och siffror och vi vill ta bort alla siffror från den. Vi kan då använda följande kod:

```Clojure
(require '[clojure.string :as str])
(str/replace "Hej123" #"\d" "") ;; output: "Hej"
```

I detta exempel använder vi funktionen `replace` från `clojure.string` biblioteket och ger den två argument - strängen vi vill manipulera och ett reguljärt uttryck `#"\d"` som betyder "matcha alla siffror". Det tredje argumentet är en tom sträng, vilket betyder att vi vill ersätta alla matchade siffror med inget, vilket resulterar i att de tas bort från strängen.

Man kan också göra mer avancerade manipulationer med reguljära uttryck för att ta bort specifika tecken eller grupper av tecken från en sträng. Det är värt att experimentera med olika uttryck för att utforska möjligheterna.

## Djupdykning

Reguljära uttryck är ett kraftfullt verktyg för strängmanipulation i Clojure. De gör det möjligt för oss att både hitta och ersätta tecken baserat på specifika mönster. Det finns också olika "modifierare" som vi kan använda tillsammans med vårt uttryck för att få mer exakta matchningar. Till exempel kan `i` modifieraren användas för att ignorera skillnaden mellan små och stora bokstäver när man söker efter matchningar.

En annan intressant funktion är `replace-first`, vilket gör att man bara tar bort det första matchande tecknet istället för alla, som `replace` gör. Det finns många andra användbara funktioner som man kan använda sig av när man arbetar med reguljära uttryck, så det kan vara värt att läsa på mer om dem.

## Se även

- [Reguljära uttryck guider för Clojure](https://clojure.org/guides/regex)
- [Official Clojure docs om `str/replace`](https://clojuredocs.org/clojure.string/replace)