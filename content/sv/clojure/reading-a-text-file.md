---
title:    "Clojure: Läsa en textfil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Varför

Att läsa textfiler är en grundläggande uppgift som många programmerare stöter på i sitt arbete. Det är viktigt att veta hur man ska hantera textfiler både för bearbetning av data och för att läsa in information i sina program.

# Så här gör du

För att läsa en textfil i Clojure kan du använda funktionen "slurp" tillsammans med filnamnet som argument. Detta kommer att läsa in hela filinnehållet som en sträng och spara den i en variabel. Till exempel:

```Clojure
(def data (slurp "min_textfil.txt"))
```

För att skriva ut innehållet i filen kan du använda "println" funktionen och skriva ut variabeln som du sparade innehållet i. Till exempel:

```Clojure
(println data)
```

Om du vill läsa in filen rad för rad kan du använda funktionen "line-seq". Denna funktion returnerar en sekvens av alla rader i filen. Du kan sedan använda "doall" funktionen för att gå igenom sekvensen och göra vad du vill med varje enskild rad. Till exempel:

```Clojure
(doall
  (line-seq (open-file "min_textfil.txt")))
```

# Djupdykning

När du läser en textfil med Clojure är det viktigt att vara medveten om hur filen är kodad. Om filen är kodad i ett annat teckenuppsättning än standard kommer du att behöva ange detta med hjälp av ":encoding" tillsammans med filnamnet. Till exempel:

```Clojure
(def data (slurp "min_textfil.txt" :encoding "UTF-8"))
```

Det är också viktigt att ta hänsyn till eventuella radbrytningar som kan finnas i filen, särskilt om du läser in filen rad för rad. I en Windowsmiljö kan radbrytningarna skilja sig från en Unixmiljö. Du kan ange hur Clojure ska hantera radbrytningarna med hjälp av ":newline" tillsammans med "line-seq" funktionen. Till exempel:

```Clojure
(doall
  (line-seq (open-file "min_textfil.txt" :newline :unix)))
```

# Se också

- [Clojure Dokumentation - File I/O](https://clojure.org/reference/java_interop#_file_io)
- [Clojure Dokumentation - Strings](https://clojure.org/reference/strings)
- [Java Dokumentation - BufferedReader Class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/BufferedReader.html)