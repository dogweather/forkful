---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML in Haskell: Eine leichte Methode

## Was & Warum?

Das Parsen von HTML bedeutet, HTML-Strings zu analysieren und sie in eine für den Computer verständliche Struktur umzuwandeln. Wir tun dies, um den Inhalt von Webseiten programmgesteuert zu manipulieren oder auszulesen.

## Wie geht's:

Um einen HTML-String in Haskell zu parsen, können wir die `html-conduit` und `xml-conduit` Bibliotheken benutzen. Im Folgenden finden Sie ein einfaches Beispiel:

```Haskell
import Text.XML.Cursor
import Network.HTTP.Conduit
import qualified Data.Text as T

main :: IO ()
main = do
  doc <- simpleHttp "https://example.com" >>= return . parseLBS_ def 
  let cursor = fromDocument doc
  print $ cursor $// element "title" &// content
```
Wenn Sie dieses Programm ausführen, lädt es die HTML-Seite von `example.com` und gibt den Inhalt des Titel-Tags aus.

## Tief eingehen

HTML-Parsing hat seinen Ursprung in der Notwendigkeit, die oft chaotische und uneinheitliche Struktur von HTML-Code in eine konsistente und leicht manipulierbare Datenstruktur zu überführen.

Als Alternative zum `html-conduit` und `xml-conduit` könnten Sie auch `tagsoup`, `html-tagsoup` oder `pandoc` benutzen, die ebenso beliebt und mächtig sind.

Im Detail verwendet unser obiger Code die Funktion `simpleHttp`, um eine HTTP-Anfrage zu senden und die Antwort als `ByteString` zurückzugeben. Dann verwendet es `parseLBS_` um diesen `ByteString` in ein `Document` zu überführen, und `fromDocument` um das `Document` zu einem `Cursor` zu machen. Jeder dieser Schritte bietet verschiedene Möglichkeiten zur Fehlersuche und Optimierung, die außerhalb des Rahmens dieses Artikels liegen.

## Siehe auch

Für weitere Informationen, durchsuchen Sie folgende Links:

1. Der Quellcode von `html-conduit`: https://github.com/snoyberg/xml
2. Eine gründlichere Einführung in `xml-conduit`: http://www.yesodweb.com/book/xml
3. Eine Präsentation über HTML-Scraping in Haskell: https://www.slideshare.net/chrisdone/scraping-html-in-haskell
4. Ein Artikel über `tagsoup`: https://neilmitchell.blogspot.com/2007/10/parsing-html-in-haskell.html

Frohes Parsen!