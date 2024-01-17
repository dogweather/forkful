---
title:                "Eine Webseite herunterladen"
html_title:           "Ruby: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, den gesamten Inhalt einer Webseite in Form von HTML-Code auf Ihren Computer oder Ihr Gerät zu übertragen. Programmierer tun dies, um Informationen von der Webseite zu extrahieren und sie für verschiedene Zwecke zu verwenden, wie z.B. Datenanalyse oder Web-Scraping.

## Wie geht's?
```Ruby
require 'open-uri'
webpage = open('https://www.example.com')
content = webpage.read
puts content
```

Die oben gezeigten Zeilen Code verwenden die Open-URI-Bibliothek, um eine Verbindung zu einer Webseite herzustellen und deren Inhalt in einer Variablen zu speichern. Dann wird der Inhalt des Webseiten-Objekts ausgegeben. Auf diese Weise können Programmierer den HTML-Code einer Webseite auf einfache Weise herunterladen.

## Tiefer Einblick
Das Herunterladen von Webseiten hat eine lange Geschichte, die bis in die Anfänge des Internets zurückreicht. Früher wurde dies hauptsächlich verwendet, um Webseiten offline verfügbar zu machen oder um Dateien von einer Webseite herunterzuladen. Heutzutage wird das Herunterladen von Webseiten häufig für Datenanalyse, Web-Scraping oder automatisierte Tests verwendet.

Es gibt auch Alternativen zu Open-URI, wie z.B. die HTTP-Bibliothek, die mehr Optionen bietet, aber auch komplexer zu nutzen ist. Wenn Sie in die Implementierungsdetails eintauchen möchten, können Sie die Dokumentation der Bibliotheken oder den Quellcode überprüfen.

## Sieh auch
- offizielle Dokumentation von Open-URI: https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html
- HTTP-Bibliothek: https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html