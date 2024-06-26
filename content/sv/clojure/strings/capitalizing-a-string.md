---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:52.865597-07:00
description: "Hur man g\xF6r: Clojure, som \xE4r ett JVM-spr\xE5k, l\xE5ter dig anv\xE4\
  nda Java String-metoder direkt. H\xE4r \xE4r ett grundl\xE4ggande exempel p\xE5\
  \ hur man f\xF6rstorar en str\xE4ng\u2026"
lastmod: '2024-03-13T22:44:37.508729-06:00'
model: gpt-4-0125-preview
summary: "Clojure, som \xE4r ett JVM-spr\xE5k, l\xE5ter dig anv\xE4nda Java String-metoder\
  \ direkt."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
Clojure, som är ett JVM-språk, låter dig använda Java String-metoder direkt. Här är ett grundläggande exempel på hur man förstorar en sträng i Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure inkluderar inte en inbyggd funktion specifikt för att förstora strängar, men som visat kan du enkelt uppnå detta genom att kombinera `clojure.string/upper-case`, `subs` och `str` funktioner.

För en mer koncis lösning och hantering av mer komplexa strängmanipulationer, kanske du vänder dig till ett tredjepartsbibliotek. Ett sådant populärt bibliotek i Clojure-ekosystemet är `clojure.string`. Dock, enligt min senaste uppdatering, erbjuder det inte en direkt `capitalize` funktion utöver vad som demonstreras med Clojures kärnfunktioner, så metoden som visas ovan är ditt raka tillvägagångssätt utan att dra in ytterligare bibliotek specifikt för förstoring.

Kom ihåg, när du arbetar med strängar i Clojure som interagerar med Java-metoder, arbetar du effektivt med Java-strängar, vilket gör att du kan utnyttja hela arsenalet av Javas String-metoder direkt i din Clojure-kod vid behov.
