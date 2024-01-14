---
title:                "Clojure: Utmatning av felsökningsdata"
simple_title:         "Utmatning av felsökningsdata"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför
När man kodar i Clojure kan det ibland vara frustrerande när något inte går som man tänkt sig. Det är då vanligt att man vill felsöka och förstå vad som egentligen händer i koden. Här kommer det att visa sig hur utskrift av debug-information kan vara till stor hjälp.

## Hur man gör
För att skriva ut debug-information i Clojure används oftast funktionen `println`. Denna funktion tar emot en eller flera argument som den skriver ut till terminalen. Låt oss titta på ett exempel:

```Clojure
(def my-var 5)
(println "Värdet av my-var är" my-var)
```
Detta kommer att skriva ut "Värdet av my-var är 5" till terminalen. Genom att lägga till utskrifter som denna på olika ställen i koden kan man få en bättre förståelse för vad som händer och varför det inte fungerar som förväntat.

## På djupet
Utöver enkla utskrifter med `println` finns det flera bibliotek och verktyg som kan hjälpa till med debugging i Clojure. Ett av dessa är [slingshot](https://github.com/scgilardi/slingshot), som ger möjlighet att skriva ut stack traces, vilket kan vara till stor hjälp för att lokalisera fel i koden. En annan användbar funktion är [clojure.inspector](https://clojure.github.io/tools.trace/api/clojure.inspector.html), som ger möjlighet att utforska och inspektera data strukturer i realtid.

## Se även
- [The ClojureScript DynamicLoader](https://medium.com/tomterence/built-in-debugging-in-clojure-clojurescript-llvm-and-cljs-ed0fc7bf0c5f)
- [Debugging symbols in Clojure](https://coderwall.com/p/e0ggbq/debugging-symbols-in-clojure)

Se även
- [The ClojureScript DynamicLoader](https://medium.com/tomterence/built-in-debugging-in-clojure-clojurescript-llvm-and-cljs-ed0fc7bf0c5f)
- [Debugging symbols in Clojure](https://coderwall.com/p/e0ggbq/debugging-symbols-in-clojure)