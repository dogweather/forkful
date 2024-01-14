---
title:                "Ruby: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Sicherlich hast du schon einmal gehört, dass es wichtig ist, deine Webanwendungen mit grundlegender Authentifizierung zu sichern. Aber hast du dich jemals gefragt, warum das notwendig ist? Die Verwendung von grundlegender Authentifizierung ermöglicht es dir, Benutzernamen und Passwörter zu überprüfen und sicherzustellen, dass nur autorisierte Benutzer Zugriff auf deine Anwendung haben. Dies ist besonders wichtig, wenn du sensible Informationen verarbeitest, wie zum Beispiel persönliche Kundeninformationen oder finanzielle Daten.

## Wie geht das?

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, musst du zuerst die `Net::HTTP` Bibliothek in deinem Ruby-Programm importieren. Dann musst du die entsprechenden Informationen in deiner Anfrage angeben, einschließlich des Benutzernamens und des Passworts. Hier ist ein Beispielcode, wie du eine GET-Anfrage mit grundlegender Authentifizierung senden kannst:

```Ruby
require 'net/http'

uri = URI('https://example.com/api')
credentials = {username: "benutzername", password: "passwort"}
response = Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https', 
                          basic_auth: credentials) do |http|
  request = Net::HTTP::Get.new uri
  http.request request # Die Anfrage wird gesendet und wir erhalten eine Antwort
end

puts response.body # Gibt die Antwort aus
```

Die `basic_auth` Option verwendet die vom Benutzer angegebenen Anmeldedaten, um die HTTP-Anfrage zu authentifizieren. Wenn die Anfrage erfolgreich ist, erhältst du eine Antwort von der angeforderten URL. In diesem Beispiel wird die Antwort als einfachen Text ausgegeben, aber du kannst sie auch in einem JSON-Objekt oder einem anderen Format erhalten, je nachdem, was deine API liefert.

## Tiefentauchen

Es gibt einige wichtige Dinge zu beachten, wenn du HTTP-Anfragen mit grundlegender Authentifizierung sendest. Zum einen ist die Verwendung von HTTPS anstelle von HTTP in der URI wichtig, um sicherzustellen, dass die Anmeldedaten sicher übertragen werden. Außerdem ist es ratsam, vor der Verwendung einer grundlegenden Authentifizierungslösung zu prüfen, ob es möglicherweise sicherere Optionen gibt, wie zum Beispiel OAuth.

Es ist auch wichtig zu beachten, dass die Übermittlung von Benutzernamen und Passwort in Klartext ein potenzielles Sicherheitsrisiko darstellt. Achte also darauf, dass du deine Anmeldedaten sicher speicherst und die Anfragen nur über eine sichere Verbindung sendest.

## Siehe auch

* ["Einführung in die Ruby Net::HTTP Bibliothek"](https://www.rubyguides.com/2018/08/ruby-net-http/)

* ["HTTP Anfragen mit Ruby"](https://www.tutorialspoint.com/ruby/ruby_http_requests.htm)

* ["Verhindern von Brute-Force Angriffen mit grundlegender Authentifizierung"](https://www.hugeserver.com/kb/preventing-brute-force-attacks-using-basic-authentication/)