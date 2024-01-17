---
title:                "Att skriva en textfil"
html_title:           "Clojure: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Att skriva en textfil innebär att skriva text till en fil som kan läsas av en dator. Det är en vanlig uppgift för programmerare eftersom textfiler är ett enkelt och populärt sätt att lagra och manipulera data.

## How to:

### Skapa en textfil
För att skapa en textfil i Clojure kan du använda funktionen `spit`, som tar emot två parametrar: en sträng som representerar filnamnet och en annan sträng med den önskade texten. Till exempel:

```Clojure
(spit "mitttextdokument.txt" "Detta är en textfil som skapades med Clojure!")
```

### Skriva till en befintlig textfil
Om du vill lägga till text till en befintlig textfil kan du använda `spit` igen, men med ett extra argument `:append true`, som talar om för funktionen att lägga till text istället för att skriva över befintlig text i filen. Till exempel:

```Clojure
(spit "mitttextdokument.txt" "Mer text som ska läggas till" :append true)
```

### Skriva flera rader till en textfil
För att skriva flera rader text till en textfil kan du använda `with-open` och `doto`. Det här mönstret av kod öppnar filen, lägger till texten och stänger sedan filen igen. Till exempel:

```Clojure
(with-open [dokument (clojure.java.io/writer "mitttextdokument.txt" :append true)]
  (doto dokument
    (clojure.data.xml/emit-str "<p>Detta är en paragraph</p>")
    (clojure.data.xml/emit-str "<p>Denna kommer på en ny rad!</p>")))
```

Output i filen blir då:

```Clojure
Detta är en textfil som skapades med Clojure!
Mer text som ska läggas till
<p>Detta är en paragraph</p>
<p>Denna kommer på en ny rad!</p>
```

## Deep Dive

### Historisk Kontext
Skrivning till textfiler är en grundläggande uppgift som varit en del av programmering sedan dagenheten. Det är också en viktig funktion för många program eftersom textfiler kan användas för att lagra data som är lättläst och hanterbar av både människor och datorer.

### Alternativ
Det finns flera andra sätt att skriva till filer i Clojure, som att använda Java-klasser som `PrintWriter` eller `FileWriter` samt funktionen `slurp` för att läsa in en hel fil som en sträng.

### Implementation detaljer
För att skriva till en fil använder Clojure sig av JVM-funktioner som `FileOutputStream` och `BufferedWriter`. För mer information kan du kolla in källkoden för `clojure.java.io/writer` och `clojure.data.xml/emit-str`.

## See Also

- [Clojure Dokumentation för spit](https://clojuredocs.org/clojure.core/spit)
- [Tutorial: "Working With Files in Clojure" by Daniel Denermon](http://daniel.denermon.com/2010/04/clojure-working-with-files.html)
- [Java Dokumentation för FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)