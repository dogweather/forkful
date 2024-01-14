---
title:                "Clojure: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av programmering i Clojure. Det tillåter utvecklare att skriva felmeddelanden som enkelt kan hittas och åtgärdas för att förbättra kodens hållbarhet och förståelse.

## Hur man gör det

Det finns flera sätt att skriva till standard error i Clojure. Det enklaste sättet är att använda funktionen `println` tillsammans med `System/err`:

```
Clojure (println "Detta är ett felmeddelande" (System/err))
```

Detta kommer att skriva ut felmeddelandet till standard error och lämna en tom rad efter.

Det finns också andra alternativ, som att använda `eprintln` eller `err-str` tillsammans med `with-out-str` för att få en sträng med felmeddelandet istället för att skriva ut det direkt. Här är ett exempel på hur man använder dessa:

```
Clojure (with-out-str (eprintln "Detta är ett felmeddelande") (flush (System/err)))
```

Detta kommer att spara strängen med felmeddelandet i en variabel och sedan skriva ut den till standard error med `flush`.

## Djupdykning

Det finns också flera funktioner som kan användas för att hantera skrivning till standard error på ett mer avancerat sätt. Till exempel kan `binding` användas för att ändra standard error-miljön temporärt, vilket kan vara användbart för feldelning och debugging. Här är ett exempel på hur `binding` kan användas för att temporärt ändra standard error-miljön:

```
Clojure (binding [*err* (java.io.PrintWriter. *out*)] (println "Detta kommer att skriva till standard error istället för standard output"))
```

Det finns också andra funktioner som `pr-str` och `pprint-str` som kan användas för att formatera utdata som ska skrivas till standard error.

## Se även

- [Officiell dokumentation för standard error](https://clojure.org/reference/lisp-features#error-handling)
- [Clojure Error Handling](https://github.com/clojure/spec-alpha/wiki/Error-Handling)
- [Skriva till standard error i Clojure](https://www.tutorialspoint.com/clojure/clojure_standard_io.htm)