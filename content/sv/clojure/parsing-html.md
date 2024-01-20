---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML är den process genom vilken vi läser och tolkar HTML-kod för att bearbeta dess struktur. Programmerare gör det för att extrahera, manipulera och interagera med data som är instängda i HTML-dokument.

## Så här gör du:
Med Clojure, använd `jsoup` bibliotek för att parsa HTML enkelt. Så här kan ditt kodblock se ut:

```Clojure
(ns your-namespace
  (:require [clojure.java.io :as io])
  (:import (org.jsoup Jsoup)))

(defn parse-html [file]
  (-> (io/file file)
      slurp
      Jsoup/parse))

(defn -main []
  (let [doc (parse-html "your-file.html")]
    (println (jsoup-title doc))))
```
Kör koden och du får titeln på ditt HTML-dokument utskrivet på konsolen.

## Djupdykning
Tidigt parsa HTML var notoriskt komplicerade och fyllda med taggfel. Men med tiden har bibliotek som `jsoup` förenklat processen. Alternativ till `jsoup` inkluderar `htmlunit` och `jtidy` men `jsoup` är favorit tack vare dess användarvänlighet. Dessutom var dess lätta tillgång till DOM-manipulation och dataextraktion idealisk i jämförelse med andra bibliotek.

## Se även
För mer information, kolla in dessa länkar:
- [Jsoup dokumentation](https://jsoup.org/)
- [Clojure dokumentation](https://clojure.org/guides/getting_started)
- [HtmlUnit](http://htmlunit.sourceforge.net/)
- [JTidy](http://jtidy.sourceforge.net/)