---
title:                "Eine http-Anfrage senden"
html_title:           "Ruby: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Senden einer HTTP-Anfrage ist eine grundlegende Methode, um mit Webservern zu kommunizieren und Informationen abzurufen oder zu senden. Programmierer verwenden diese Technik, um Webanwendungen zu erstellen, Daten von APIs zu erhalten oder einfach nur Websites aufzurufen.

# Wie geht's?
```Ruby
require 'net/http' # Wir müssen das net/http-Modul importieren

# Ein Beispiel für eine GET-Anfrage
uri = URI('https://example.com/') # Wir geben die URL der gewünschten Seite an
response = Net::HTTP.get(uri) # Wir verwenden die get-Methode, um eine Antwort von der Seite zu erhalten
puts response # Wir geben die empfangene Antwort aus

# Ein Beispiel für einen POST-Antrag mit Daten
uri = URI('https://example.com/') # Wir geben wieder die URL an
params = { username: "John", password: "secret" } # Wir erstellen ein Hash-Objekt mit den zu sendenden Daten
response = Net::HTTP.post_form(uri, params) # Wir verwenden die post_form-Methode, um die Daten zu senden und eine Antwort zu erhalten
puts response # Wir geben die empfangene Antwort aus
```

# Tiefere Einblicke
Das HTTP-Protokoll wurde in den späten 80er Jahren entwickelt und hat seitdem viele Iterationen und Verbesserungen erfahren. Es ist das grundlegende Kommunikationsprotokoll im World Wide Web und wird von allen gängigen Webanwendungen verwendet. Es gibt auch andere Methoden, um mit Webservern zu kommunizieren, wie z.B. das SMTP-Protokoll für E-Mail-Kommunikation.

# Sieh dir auch an
- [Net::HTTP-Dokumentation](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial: HTTP-Anfragen mit Ruby](https://www.rubyguides.com/2018/08/ruby-http-request/)