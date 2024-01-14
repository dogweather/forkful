---
title:                "Ruby: Versenden einer http Anfrage"
simple_title:         "Versenden einer http Anfrage"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich mit dem Versenden von HTTP-Anfragen beschäftigen? Nun, in der heutigen digitalen Welt sind Webanwendungen und APIs allgegenwärtig. Das Verständnis der grundlegenden Mechanismen von HTTP-Anfragen kann Ihnen helfen, diese Technologien besser zu verstehen und möglicherweise sogar Ihre eigene Anwendung zu entwickeln.

# Wie geht's

Um eine HTTP-Anfrage mit Ruby zu senden, müssen Sie das Net::HTTP-Modul verwenden, das Teil der Standardbibliothek von Ruby ist. In den folgenden Code-Beispielen stellen wir Ihnen zwei verschiedene Methoden vor, wie Sie eine einfache HTTP-GET-Anfrage senden können.

````ruby
require 'net/http'

# Methode 1: Net::HTTP#start

# Das Ziel-URL als Zeichenkette speichern
url_string = "https://www.example.com"

# Eine neue HTTP-Verbindung erstellen und mit dem Server verbinden
response = Net::HTTP.get_response(URI(url_string))

# Den Antwort-Code ausgeben
puts "Antwort-Code: #{response.code}"
# => Antwort-Code: 200

# Die zurückgegebene Daten lesen und ausgeben
puts "Antwort-Daten: #{response.body}"
# => Antwort-Daten: <html>...</html>

# Methode 2: Net::HTTP.get
# Diese Methode hat weniger Schritte und ermöglicht es Ihnen, die URL direkt an die Methode zu übergeben.

url_string = "https://www.example.com"

# Eine kurze und saubere Methode, um eine GET-Anfrage zu senden
response = Net::HTTP.get(URI(url_string))

# Den Antwort-Code ausgeben
puts "Antwort-Code: #{response.code}"
# => Antwort-Code: 200

# Die zurückgegebene Daten lesen und ausgeben
puts "Antwort-Daten: #{response.body}"
# => Antwort-Daten: <html>...</html>
````

# Deep Dive

Wenn Sie genauer in die Details des Sendens von HTTP-Anfragen eintauchen möchten, gibt es noch einige weitere Aspekte, die Sie beachten sollten. Zum Beispiel können Sie mit der Net::HTTP-Klasse auch andere HTTP-Methoden wie POST, PUT oder DELETE verwenden. Sie können auch Header-Informationen hinzufügen oder die Verbindung mit HTTP-Platzierungsoptionen steuern.

Es ist auch wichtig zu verstehen, dass HTTP-Anfragen und -Antworten in Form von Text statt Binärdaten erfolgen. Dies ermöglicht es uns als Entwickler, die Daten in lesbarer Form zu analysieren und zu verarbeiten. Aber es bedeutet auch, dass wir spezielle Schritte unternehmen müssen, um Dateien oder Bilder über HTTP-Anfragen zu übertragen, z. B. durch die Verwendung von Base64-Codierung.

# Siehe auch

- [Net::HTTP Dokumentation](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/index.html)
- [Ruby HTTP-Bibliotheken (auf Deutsch)](https://stackify.com/ruby-http-client-libraries/)
- [Einführung in REST und HTTP](https://www.atlassian.com/de/git/tutorials/what-is-http)