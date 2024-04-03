---
date: 2024-01-20 17:54:21.637050-07:00
description: "Hur man g\xF6r: L\xE5t oss dyka rakt in i koden f\xF6r att l\xE4sa en\
  \ textfil i Clojure."
lastmod: '2024-03-13T22:44:37.542357-06:00'
model: gpt-4-1106-preview
summary: "L\xE5t oss dyka rakt in i koden f\xF6r att l\xE4sa en textfil i Clojure."
title: "L\xE4sa en textfil"
weight: 22
---

## Hur man gör:
Låt oss dyka rakt in i koden för att läsa en textfil i Clojure:

```clojure
;; Läs hela filen på en gång
(slurp "exempel.txt")

;; Läs filen rad för rad
(with-open [r (clojure.java.io/reader "exempel.txt")]
  (doall (line-seq r)))
```

Om "exempel.txt" innehåller:
```
Hej världen!
Clojure är kul.
```

Så skulle output bli:
```
"Hej världen!\nClojure är kul.\n"
```
för `(slurp "exempel.txt")` och 
```
("Hej världen!" "Clojure är kul.")
```
för den andra varianten.

## Djupdykning
Att läsa textfiler är grundläggande och har gjorts sedan programmeringens barndom. Clojure, en modern Lisp-variant, hanterar detta enkelt med inbyggda funktioner som `slurp` och `line-seq`. `slurp` är bra för mindre filer då den läser hela innehållet på en gång. `line-seq` kombinerat med `with-open` är bättre för stora filer då det läser rad för rad och därmed använder mindre minne.

Alternativ till dessa metoder inkluderar användning av Java-bibliotek direkt genom interop, som kan vara mer kraftfullt för avancerad filhantering eller när prestanda är kritisk.

Implementationen av filinläsning i Clojure kan verka magisk, men under huven anropar dessa funktioner Java-bibliotek, vilket innebär att Clojure drar nytta av Javas mognad och effektivitet.

## Se även
- [Clojure Documentation for slurp](https://clojuredocs.org/clojure.core/slurp)
- [Clojure Documentation for line-seq](https://clojuredocs.org/clojure.core/line-seq)
- [Clojure java.io API](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Practical Clojure (Bok)](http://www.pragprog.com/titles/shcloj2/practical-clojure)
