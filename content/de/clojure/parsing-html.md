---
title:                "HTML-Analyse"
html_title:           "Clojure: HTML-Analyse"
simple_title:         "HTML-Analyse"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Ähnlich wie in anderen Programmiersprachen, ist das Analysieren von HTML in Clojure nützlich für die webbasierte Entwicklung. Es ermöglicht dem Benutzer, strukturierte Daten aus einer Webseite zu extrahieren und sie in ihrem Programmcode zu verwenden.

## Wie du das machst

Im Folgenden findest du einige Beispiele, wie du HTML in Clojure parsen kannst:

```Clojure
(def input "<html><head><title>Meine Webseite</title></head><body><h1>Willkommen!</h1></body></html>")
(def output (org.jsoup.Jsoup/parse input))
(println (:text (get-in output [:head :title])))
```

Dieses Beispiel nutzt die Bibliothek "jsoup" und extrahiert den Titel aus dem HTML-Code, indem es das DOM-Datenmodell durchläuft. Die Ausgabe wird "Meine Webseite" sein.

```Clojure
(def input "<div><span id="user">John</span></div>")
(def output (org.jsoup.Jsoup/parse input))
(println (:text (get-in output [:div :span])))
```

Hier wird der Benutzername "John" aus dem div-Element extrahiert und ausgegeben.

## Tiefer ins Detail

Das Parsen von HTML in Clojure kann auf verschiedene Arten erreicht werden, abhängig von den Anforderungen des Benutzers. Einige Entwickler verwenden möglicherweise die Bibliothek "clj-html-parser", während andere die Kombination von "jsoup" und "Enlive" bevorzugen.

Enlive ist eine spezielle Bibliothek für die Manipulation und Extraktion von HTML in Clojure. Es bietet eine einfachere Syntax für die Navigation im DOM und unterstützt auch das Templating.

Es ist auch möglich, mit regulären Ausdrücken durch den HTML-Code zu navigieren, aber dies wird im Allgemeinen nicht empfohlen, da HTML-Code sehr komplex und schwer zu verarbeiten sein kann.

## Siehe auch

- [Offizielle Clojure-Dokumentation](https://clojuredocs.org/clojure.core/split)
- [jsoup Bibliothek](https://jsoup.org/)
- [Enlive Bibliothek](https://github.com/cgrand/enlive)