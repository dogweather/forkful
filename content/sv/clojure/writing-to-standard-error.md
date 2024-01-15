---
title:                "Att skriva till standard error"
html_title:           "Clojure: Att skriva till standard error"
simple_title:         "Att skriva till standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfelutmatning (standard error) är en användbar funktion för felsökning och debugging i Clojure. Genom att skriva felmeddelanden till standard error istället för standardutmatningen kan man enklare skilja mellan utmatning av information och felmeddelanden.

## Hur man gör

För att skriva till standard error i Clojure, använder man funktionen `print-err`. Denna funktion tar emot en eller flera argument och skriver dem till standard error. 

```Clojure
(print-err "Detta är ett felmeddelande")
```

Detta kommer att skriva ut strängen "Detta är ett felmeddelande" till standard error. Om vi skulle använda funktionen `print` istället, skulle strängen skrivas ut till standardutmatningen. 

Man kan även använda `print-err` med flera argument, precis som `print`. Detta är användbart om man vill skriva ut flera värden eller variabler i samma felmeddelande. 

```Clojure
(def name "Lisa")
(def age 25)
(print-err "Användare" name "är" age "år gammal.")
```

Detta skulle skriva ut "Användare Lisa är 25 år gammal." till standard error. 

## Djupdykning

När man skriver till standard error i Clojure, skriver man faktiskt till en Java-stream som heter `System.err`. Därför anropar funktionen `print-err` i själva verket `System.err.print`. Detta betyder att man också kan använda Java-metoder för att skriva till standarderror. En vanlig metod är att använda `println`, som automatiskt lägger till ett radbryt efter värdena som skrivs ut. 

```Clojure
(System/err/println "Detta är ett felmeddelande")
```

Man kan också formatera utskriften genom att använda `System.err/format`, som fungerar på samma sätt som `clojure.core/format`. 

## Se även

- [Java I/O streams](https://www.javatpoint.com/file-output-stream)
- [Clojure Standard Library](https://clojuredocs.org/clojure.core/print-err)