---
title:                "HTML parsen"
date:                  2024-01-20T15:31:12.710558-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bedeutet, den Code einer Webseite zu analysieren und die enthaltenen Daten zu extrahieren. Programmierer tun dies, um Webinhalte für verschiedene Anwendungen zugänglich und nutzbar zu machen.

## How to:
Mit Fish Shell und einigen Tools kannst du mühelos HTML-Inhalte parsen. Hier sind einige Code-Snippets, die dir den Einstieg erleichtern:

```fish
# HTML einer Seite mit curl herunterladen
set url "http://example.com"
set html_content (curl -s $url)

# HTML mit pup parsen -- ein Befehlszeilen-Tool für das Parsen von HTML
echo $html_content | pup 'p text{}'

# Beispiel-Output:
# Das ist ein Beispieltext auf einer Webseite.
```

Stelle sicher, dass du `pup` installiert hast. Falls nicht, installiere es mit dem Befehl:

```fish
brew install pup
```

## Tiefgang
Das Parsen von HTML hat schon immer eine wichtige Rolle gespielt, seitdem das Web in den frühen 90ern aufkam. Es ermöglichte das automatische Abrufen von Informationen, lange bevor APIs allgegenwärtig wurden. Heute gibt es robustere Tools wie BeautifulSoup für Python oder Nokogiri für Ruby. Im Vergleich dazu bietet Fish mit Bordmitteln kein spezialisiertes Parsing, aber mit der Kombination aus Unix-Tools wie `sed`, `awk`, `grep` und anderen (wie `pup`) lässt sich dennoch effizient arbeiten.

Detailinfos:
- HTML ist nicht immer sauber strukturiert oder gültig. Daher ist ein flexibles Parsen notwendig.
- Sicherheitseinschränkungen (wie bösartiges HTML) müssen berücksichtigt werden.
- Performance kann bei großen HTML-Dokumenten eine Herausforderung sein.

## Siehe Auch:
- Offizielle `pup` Dokumentation: https://github.com/ericchiang/pup
- HTML Parsing mit BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/
- W3C HTML Validator für gültiges HTML: https://validator.w3.org/