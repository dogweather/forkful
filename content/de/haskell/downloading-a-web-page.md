---
title:                "Webseite herunterladen"
html_title:           "Haskell: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Webseiten zu downloaden bedeutet, dass man den Inhalt einer Webseite herunterlädt. Programmierer tun dies, um den Inhalt einer Webseite für ihre Anwendungen oder Analysen zu nutzen.

## Wie:

```Haskell
import Network.HTTP.Simple

main = do
  request <- parseRequest "GET https://www.example.com"
  response <- httpLBS request
  print $ getResponseBody response
```

Das obige Beispiel zeigt, wie man mit Hilfe der "HTTP Simple" Bibliothek in Haskell eine GET-Anfrage an eine Webseite stellt und den Inhalt der Antwort ausgibt. Zum Ausführen wird ein GHC Compiler benötigt.

### Ausgabe:
```
"<!doctype html>\n<html>\n<head>\n<title>Example Domain</title>\n<meta charset=\"utf-8\" />\n<meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n<style type=\"text/css\">\nbody {\n\tbackground-color: #f0f0f2;\n\tmargin: 0;\n\tpadding: 0;\n\tfont-family: \"Open Sans\", \"Helvetica Neue\", Helvetica, Arial, sans-serif;\n\t\n}\ndiv {\n\twidth: 600px;\n\tmargin: 5em auto;\n\tpadding: 50px;\n\tbackground-color: #fff;\n\tborder-radius: 1em;\n}\na:link, a:visited {\n\tcolor: #38488f;\n\ttext-decoration: none;\n}\na:hover, a:active {\n\tcolor: #ff0000;\n}\n</style>    \n</head>    \n\n<body>  \n<div>\n<h1>Example Domain</h1>\n<p>This domain is established to be used for illustrative examples in documents. You may use this\n    domain in examples without prior coordination or asking for permission.</p>\n<p><a href=\"https://www.iana.org/domains/example\">More information...</a></p>\n</div>\n</body>\n</html>\n"
```

## Tiefere Einblicke:

Webseitenfinale dient nicht nur zum Anzeigen von Inhalten, sondern kann auch verwendet werden, um Daten oder Informationen aus dem Internet zu sammeln. Alternativen zu der von uns verwendeten "HTTP Simple" Bibliothek sind beispielsweise "HTTP Conduit" oder die "Network.HTTP" Bibliothek. Es ist wichtig zu beachten, dass beim Herunterladen von Webseiten auch Sicherheitsrisiken bestehen und daher sollte immer auf vertrauenswürdige Quellen vertraut werden.

## Siehe auch:

- [HTTP Simple Dokumentation](https://hackage.haskell.org/package/http-simple)
- [HTTP Conduit GitHub Repository](https://github.com/snoyberg/http-kit)
- [Network.HTTP Dokumentation](https://hackage.haskell.org/package/network)