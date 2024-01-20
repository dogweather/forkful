---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att spara information i en fil på din dator. Programmerare gör detta för att behålla data mellan sessioner, exempelvis konfigurationsfiler, loggar eller användarinput.

## Hur man gör:
```Clojure
(with-open [writer (java.io.BufferedWriter. (java.io.FileWriter. "hello.txt"))]
  (.write writer "Hej, det här är text i en fil på svenska!"))
```
Efter kodexekveringen finns en fil `hello.txt` med texten "Hej, det här är text i en fil på svenska!".

## Fördjupning
Förr lagrades data ofta i textfiler, men idag finns databaser och andra format, som JSON och XML. `with-open` i Clojure ser till att filen stängs korrekt efter användning, vilket är viktigt för resurshantering. Man kan också använda `spit` och `slurp` för enklare filhantering.

## Se även
- [Clojure Documentation on I/O](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [ClojureDocs: spit](https://clojuredocs.org/clojure.core/spit)