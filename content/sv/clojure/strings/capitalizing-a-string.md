---
title:                "Gör om en sträng till versaler"
date:                  2024-02-03T19:04:52.865597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att förstora en sträng innebär att ändra strängen så att dess första tecken är en versal, medan resten av strängen förblir oförändrad. Programmerare utför ofta strängförstoring för att säkerställa datakonsekvens, särskilt för namn och platser eller för att följa grammatiska regler i användargränssnitt.

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
