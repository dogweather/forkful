---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Webseite herunterladen bedeutet, die Daten einer Webseite von ihrem Server auf deinen lokalen Computer zu übertragen. Programmierer brauchen diese Funktion oft für Web-Scraping oder Datenextraktion.

## Anleitung:

Um in Haskell eine Webseite herunterzuladen, nutzen wir das Paket `http-conduit`. Hier ist ein einfacher code dazu:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    putStrLn "Which page do you want to download?"
    url <- getLine
    content <- simpleHttp url
    putStrLn content
```
Wenn Du das Programm startest und eine URL eingibst, z.B. https://www.example.com, es sollte dann den Inhalt der Webseite zur Konsole ausgeben.

## Tiefer Eintauchen:

Die Fähigkeit, Webseiten herunterzuladen, hat eine lange Geschichte in der Programmierung, da es im Kern des Internets steht. Frühere Sprachen, sogar C, hatten Möglichkeiten, dies durch Sockets und HTTP-Anfragen über TCP/IP zu erreichen. Allerdings ist `http-conduit` eine der häufigsten und einfachsten Funktionen, die in Haskell verwendet wird.

Es gibt auch Alternativen zu `http-conduit`, wie `http-client` oder `wreq`, die jeweils ihre eigenen Vorteile und Nachteile haben.

Die `simpleHttp`-Funktion in unserem Beispiel handhabt viele Details im Hintergrund. Sie öffnet eine HTTP-Verbindung, sendet eine GET-Anfrage an den Server, empfängt die Antwort, liest die Daten und schließt die Verbindung.

## Weiterführende Literatur:

Weitere Informationen zu diesem Thema findest du hier:

- Offizielle Dokumentation für `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Haskell Wiki Artikel über HTTP-Verbindungen: https://wiki.haskell.org/Web/Http
- Blogpost über das Herunterladen von Web-Inhalten in Haskell: https://www.fpcomplete.com/haskell/library/http-conduit/