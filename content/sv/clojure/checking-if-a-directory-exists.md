---
title:                "Kontrollera om en mapp existerar"
html_title:           "Clojure: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med filer och mappar i ditt Clojure-program kan det vara viktigt att kunna kontrollera om en viss mapp finns eller inte. Det kan hjälpa dig att undvika problem och se till att ditt program fungerar korrekt.

## Hur man gör

Det finns två sätt att kontrollera om en mapp finns i din Clojure-kod, beroende på vilken version du använder. Vi kommer att gå igenom båda alternativen nedan.

### För Clojure 1.9 och högre

För att kontrollera om en mapp finns i ditt system, använd funktionen `file?` tillsammans med `file-symlink?` för att hantera symboliska länkar. Här är ett exempel på hur du kan göra det:

```Clojure
;; Skapa en väg till mappen du vill kontrollera
(def path "/home/user/documents/")

(println (file? (io/file path))) ; => true om mappen finns
(println (file-symlink? (io/file path))) ; => false om mappen inte är en symbolisk länk
```

Om du får "false" som svar på båda anropen betyder det att mappen inte finns eller att den är en symbolisk länk.

### För äldre Clojure-versioner

Om du använder en äldre version av Clojure, innan `file?` och `file-symlink?` fanns i språket, kan du använda funktionen `file-seq` för att lista alla filer och mappar i en viss sökväg. Sedan kan du kontrollera om namnet på mappen finns i den listan. Här är ett exempel:

```Clojure
;; Skapa en väg till mappen du vill kontrollera
(def check-dir "/home/user/documents/")

;; Hämta lista över filer och mappar i sökvägen
(def dir-list (file-seq (io/file check-dir)))

;; Kontrollera om mappen finns i listan
(if (some #(= (:name %) (io/file check-dir)) dir-list)
  (println "Mappen finns")
  (println "Mappen finns inte"))
```

Om du får "Mappen finns" som svar betyder det att mappen finns på den angivna sökvägen.

## Djupdykning

I vissa fall kanske du inte bara vill kontrollera om en mapp finns, utan också få mer information om den. I så fall kan du använda funktionen `file-info` tillsammans med `file?` för att få tillgång till metadata om mappen. Här är ett exempel:

```Clojure
;; Skapa en väg till mappen du vill undersöka
(def path "/home/user/documents/")

;; Kontrollera om mappen finns
(if (file? (io/file path))
  ;; Hämta metadata om mappen
  (let [info (file-info (io/file path))]
    (println "Namn:" (.getName info)) ; => Namnet på mappen
    (println "Storlek:" (.length info)) ; => Storlek i bytes
    (println "Skapad:" (.creationTime info)) ; => Datum då mappen skapades
    (println "Senast ändrad:" (.lastModified info))) ; => Datum då mappen senast ändrades
  (println "Mappen finns inte"))
```

## Se även

- [Clojure dokumentation: io/file](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/io)
- [Clojure dokumentation: file-info](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file-info)