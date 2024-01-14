---
title:                "Ruby: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren mit Ruby gibt es unzählige Möglichkeiten, um Daten im Internet abzurufen. Sei es für das Scraping von Informationen, das Durchsuchen von Webseiten oder die Verwendung von APIs, der Download einer Webseite kann ein nützliches Werkzeug sein. In diesem Blogbeitrag werden wir uns genauer damit beschäftigen, wie man eine Webseite mit Ruby herunterladen kann.

## Wie funktioniert es?

Zunächst müssen wir die erforderlichen Gems installieren, um die Webseite herunterladen zu können. Ein weit verbreitetes Gem ist "net/http", das uns die nötigen Funktionen bereitstellt.

```
```Ruby
require 'net/http'
```

Als nächstes definieren wir die URL der Webseite, die wir herunterladen möchten.

```Ruby
url = 'https://www.example.com'
```

Wir verwenden dann die Methode `get`, um eine HTTP GET-Anfrage an die angegebene URL zu senden und die Antwort in einer Variable zu speichern.

```Ruby
response = Net::HTTP.get_response(URI(url))
```

Um den Inhalt der Webseite anzuzeigen, können wir die Methode `body` verwenden.

```Ruby
puts response.body
```

Damit haben wir erfolgreich eine Webseite mit Ruby heruntergeladen!

## Tiefergehende Einblicke

Es gibt viele verschiedene Funktionen und Möglichkeiten, um eine Webseite mit Ruby herunterzuladen. Zum Beispiel können wir auch eine Datei direkt auf unseren Computer speichern, anstatt sie nur im Terminal anzuzeigen. Dazu müssen wir die Response in einen Stream umwandeln und den Inhalt der Datei in diesen Stream schreiben.

```Ruby
# Öffne die Datei zum Schreiben
file = File.open('index.html', 'w')

# Definiere den Stream
stream = response.body

# Schreibe den Inhalt der Datei in den Stream
while data = stream.read(4096)
  # Schreibe die Daten in die Datei
  file.write(data)
end

# Schließe die Datei
file.close
```

Diese Methode ermöglicht es uns, die heruntergeladene Webseite auch offline zu nutzen.

## Siehe auch

- Ruby Documentation: https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html
- Tutorial: https://www.rubyguides.com/2016/07/ruby-web-scraping-libraries/