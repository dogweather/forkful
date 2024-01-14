---
title:                "Clojure: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog finns är en viktig del av programmering, eftersom det låter oss säkerställa att vår kod fungerar korrekt och att alla nödvändiga resurser finns tillgängliga. I denna bloggpost kommer vi att utforska hur man gör detta i Clojure och varför det är en viktig del av utvecklingsprocessen.

## Så här gör du

Det finns flera sätt att kontrollera om en katalog finns i Clojure. Ett enkelt sätt är att använda funktionen `file-seq`, som kommer att returnera en sekvens av filer och kataloger i en given sökväg. Om en katalog finns kommer den att vara en del av denna sekvens.

```Clojure
(def dir-path "testdir")

(file-seq dir-path)

; => ("testdir/file1.txt" "testdir/file2.txt" "testdir/subdir1" "testdir/subdir2")
```

Vi kan också använda funktionen `file?` för att kontrollera om en given sökväg är en fil eller en katalog. Om sökvägen är en katalog kommer funktionen att returnera `true`.

```Clojure
(def dir-path "testdir")

(file? dir-path)

; => true
```

Om du vill kontrollera om en katalog finns på en specifik sökväg, kan du använda funktionen `exists?`, som returnerar `true` om sökvägen existerar och annars `false`.

```Clojure
(def dir-path "testdir")

(exists? dir-path)

; => true
```

## Djupdykning

Att kontrollera om en katalog finns är en viktig del av säkerställande att vår kod fungerar som den ska. Det kan spara oss mycket tid och frustration genom att upptäcka eventuella problem tidigt i utvecklingsprocessen. Vi kan också använda dessa funktioner för att skapa nya kataloger om de inte redan finns, vilket kan vara användbart när man skapar strukturer för filer och kataloger.

## Se även

- Clojure API-dokumentation för `file-seq`: https://clojuredocs.org/clojure.java.io/file-seq
- Clojure API-dokumentation för `file?`: https://clojuredocs.org/clojure.java.io/file_q
- Clojure API-dokumentation för `exists?`: https://clojuredocs.org/clojure.java.io/exists_q