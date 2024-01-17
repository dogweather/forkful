---
title:                "HTML-Parsing"
html_title:           "Javascript: HTML-Parsing"
simple_title:         "HTML-Parsing"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Was und Warum?
Beim Parsen von HTML geht es darum, den Code einer HTML-Seite zu analysieren und daraus nützliche Informationen zu extrahieren. Programmierer nutzen das Parsen, um zum Beispiel bestimmte Inhalte oder Formatierungen aus einer Webseite zu ziehen und weiterzuverarbeiten.

## Wie geht das?
Um HTML zu parsen, können wir die Javascript Bibliothek "HTMLParser" verwenden. Dies ermöglicht es uns, den HTML Code in ein Document Object Model (DOM) umzuwandeln. So können wir auf bestimmte Elemente und Attribute zugreifen und sie manipulieren.

```Javascript
const parser = new HTMLParser();
const html = '<div>Willkommen auf meiner Webseite</div>';
const dom = parser.parse(html);
console.log(dom.getElementsByTagName('div')[0].innerHTML);
// Ausgabe: "Willkommen auf meiner Webseite"
```

## Tiefere Einblicke
Das Parsen von HTML hat sich im Laufe der Zeit weiterentwickelt. Früher wurde dies vor allem auf serverseitiger Ebene erledigt, um dynamische Webseiten zu generieren. Heutzutage wird es häufig auf clientseitiger Ebene durchgeführt, um interaktive Funktionen zu ermöglichen. Es gibt auch alternative Möglichkeiten, um HTML zu parsen, wie zum Beispiel mit regulären Ausdrücken, aber dies kann schnell unübersichtlich und fehleranfällig werden.

## Siehe auch
- [HTMLParser Dokumentation](https://htmlparser.org/)
- [Einführung in DOM Manipulation mit Javascript](https://developer.mozilla.org/de/docs/Web/API/Document_Object_Model/Introduction)
- [Alternativen zum Parsen von HTML](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454)