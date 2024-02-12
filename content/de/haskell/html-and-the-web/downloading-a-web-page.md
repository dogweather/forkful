---
title:                "Webseite herunterladen"
aliases:
- /de/haskell/downloading-a-web-page.md
date:                  2024-01-20T17:44:14.647607-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, den Inhalt der Seite über das Internet zu beziehen, um ihn lokal zu verarbeiten oder zu analysieren. Programmierer tun dies für Aufgaben wie Datenscraping, Automatisierungstests oder um Inhalte offline verfügbar zu machen.

## So geht's:
Installiere die `http-conduit` Bibliothek mit Cabal oder Stack. Dann kannst du mit wenigen Zeilen Code eine Webseite herunterladen. Beispiel:

```haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let body = getResponseBody response
    putStrLn $ "The first 60 characters of the response are: " ++ take 60 (unpack body)
```

Starte dein Programm. Die erste Zeile des Outputs sollte in etwa so aussehen:

```
The first 60 characters of the response are: <!doctype html>...
```

## Tiefergehende Einblicke:
`http-conduit` ist Teil des größeren Conduit-Bibliothekenpaketes, das auf Streams und ressourceneffizienten Datenfluss ausgerichtet ist. Alternativen wie `wreq` oder `curl` sind ebenfalls populär, haben aber ihre eigenen Stärken und Schwächen. Beim Herunterladen einer Webseite geht es nicht nur darum, die Daten zu erhalten – es muss auch der Statuscode geprüft, Fehler behandelt und oft auch mit Headers oder Cookies gearbeitet werden. 

`http-conduit` macht vieles unter der Haube: es handhabt Netzwerkverbindungen, SSL-Verschlüsselung und bietet einfache Funktionen für häufige HTTP-Operationen. Es ist eine gute Mischung aus Einfachheit und Kontrolle – perfekt für Skripte und auch größere Anwendungen.

## Siehe auch:
- [`http-conduit` on Hackage](https://hackage.haskell.org/package/http-conduit)
- [Conduit documentation on Hackage](https://hackage.haskell.org/package/conduit)
- [Official `wreq` GitHub repository](https://github.com/bos/wreq)
- [The `curl` library on Hackage](https://hackage.haskell.org/package/curl)
