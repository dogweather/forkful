---
title:                "Läsa en textfil"
html_title:           "Clojure: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är helt enkelt att ta in innehållet i en textfil och göra det tillgängligt för din dator att läsa och behandla. Det kan vara användbart för programmörer att läsa textfiler för att kunna bearbeta och använda dess innehåll i sina program.

Att kunna läsa och behandla textfiler ger programmörer en större flexibilitet i sina program, då de kan använda sig av befintliga filer med information istället för att behöva skriva in all information manuellt. Det gör också möjligt att integrera data från externa källor och därmed utöka funktionaliteten i programmen.

## Så här gör du:
För att läsa en textfil i Clojure används funktionen ```slurp```, som tar in filnamnet som argument och returnerar texten i filen som en sträng. Se exempel nedan:

```Clojure
(def text-innehåll (slurp "filnamn.txt"))
(println text-innehåll)
```

```filnamn.txt``` innehåller följande text:

```
Hej på dig!
Jag är en textfil.
```

Output:

```
Hej på dig!
Jag är en textfil.
```

Du kan också läsa in textfilen rad för rad med hjälp av funktionen ```line-seq```:

```Clojure
(def rader (line-seq (clojure.java.io/reader "filnamn.txt")))
(doseq [rad rader]
  (println rad))
```

Output:

```
Hej på dig!
Jag är en textfil.
```

## Gräv Djupare:
Det finns olika sätt att läsa en textfil i Clojure, men ```slurp``` är den enklaste och mest använda. En annan funktion som kan användas är ```read-string```, som läser in en sträng och returnerar en datastruktur. Det finns också tredjepartsbibliotek som erbjuder avancerade funktioner för att läsa och bearbeta textfiler.

Att läsa textfiler var mycket vanligare förr i tiden, då det var den främsta metoden för att lagra och dela information. Nu för tiden används ofta andra format, som JSON och XML, men textfiler är fortfarande användbara för vissa ändamål, som att lagra loggar och enkla konfigurationsfiler.

## Se även:
Officiell Clojure dokumentation för ```slurp``` och ```read-string```:
https://clojure.org/api/cheatsheet

Third party bibliotek för att hantera textfiler:
https://github.com/clojure/data.json
https://github.com/noisesmith/edn-format