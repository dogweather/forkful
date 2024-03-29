---
date: 2024-01-20 17:34:24.380582-07:00
description: "I programmering inneb\xE4r 'konkatenering av str\xE4ngar' att sm\xE4\
  lta samman tv\xE5 eller fler textstycken. Vi g\xF6r det f\xF6r att skapa meningsfulla\
  \ meddelanden, bygga\u2026"
lastmod: '2024-03-13T22:44:37.517413-06:00'
model: gpt-4-1106-preview
summary: "I programmering inneb\xE4r 'konkatenering av str\xE4ngar' att sm\xE4lta\
  \ samman tv\xE5 eller fler textstycken. Vi g\xF6r det f\xF6r att skapa meningsfulla\
  \ meddelanden, bygga\u2026"
title: "Sammanslagning av str\xE4ngar"
---

{{< edit_this_page >}}

## Vad & Varför?
I programmering innebär 'konkatenering av strängar' att smälta samman två eller fler textstycken. Vi gör det för att skapa meningsfulla meddelanden, bygga upp dynamiska SQL-frågor eller skicka strukturerad data.

## Så här gör du:
I Clojure kan du konkatenera strängar med `str` funktionen. Här är ett enkelt exempel:

```clojure
;; Konkatenera två strängar
(str "Hej, " "världen!")
;; => "Hej, världen!"
```

Du kan lägga ihop flera strängar och till och med blanda in andra datatyper som kommer att konverteras till strängar:

```clojure
;; Blanda strängar och siffror
(str "Fibonacci-tal: " 1 ", " 1 ", " 2 ", " 3 ", " 5)
;; => "Fibonacci-tal: 1, 1, 2, 3, 5"
```

Sammanslagen av tomma strängar eller null-värden:

```clojure
;; Hantera tomma strängar och null
(str "Tystnad före " nil " stormen.")
;; => "Tystnad före  stormen."
```

## På djupet:
Tillbaka i tiden, i Lisp-dialekter före Clojure, var konkatenering ofta gjord med funktioner som `concat` som fungerade på listor snarare än strängar. Med tiden utvecklades `str`, en högre nivå funktion specifikt för strängkonkatenering.

Alternativ till `str` inkluderar att använda `format` för mer kontrollerade format eller `StringBuilder` för prestandakritiska applikationer där många strängar ska sammanfogas.

När det gäller implementering använder `str` under huven Java's `StringBuilder` för att effektivisera processen.

Exempel med `format`:

```clojure
(format "Han sa: %s" "Hello, world!")
;; => "Han sa: Hello, world!"
```

Med `StringBuilder`:

```clojure
(let [sb (StringBuilder.)]
  (.append sb "Introduktion: ")
  (.append sb "Clojure")
  (.append sb " rocks!")
  (str sb))
;; => "Introduktion: Clojure rocks!"
```

## Se även:
- Clojure Docs: [str](https://clojuredocs.org/clojure.core/str)
- Java Docs: [StringBuilder](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- Clojure från början – en lättsmält guide: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
