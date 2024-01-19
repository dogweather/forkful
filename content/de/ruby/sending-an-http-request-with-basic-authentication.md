---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anforderung mit grundlegender Authentifizierung ist der Prozess, einen gesicherten Datenaustausch zwischen Client und Server durchzuführen. Dies wird von Programmierern genutzt, um Daten sicher zu transportieren und einen nicht-autorisierten Zugang zu verhindern.

## So geht's:

Verwende die eingebauten "net/http" und "uri" Bibliotheken in Ruby:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/path')

req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body
```

Wenn du einen Benutzernamen und ein Passwort zu deiner Anforderung hinzufügst, werden diese Informationen im Header der Anforderung gespeichert. Beim Empfang dieser Anforderung wird der Server die Authentifizierungsangaben prüfen und entscheiden, ob er die angeforderten Daten zurückschickt oder die Anforderung ablehnt.

## Vertiefung:

Die grundlegende Authentifizierung ist ein Standardmechanismus, den das HTTP-Protokoll zur Authentifizierung von Benutzern bereitstellt. Obwohl es sich um eine ältere Methode handelt (erstmals in den 90er Jahren eingeführt), ist sie immer noch weit verbreitet, vor allem wegen ihrer Einfachheit.

Es existieren auch alternative Authentifizierungsmethoden, wie die "Digest"-Authentifizierung oder das modernere "OAuth". Wie bei allem hat jede Methode ihre Vor- und Nachteile und die Wahl hängt von den spezifischen Bedürfnissen deines Projekts ab.

Die Implementierung dieses Features in Ruby ist unkompliziert und effizient, da die grundlegende Authentifizierung bereits von der Standardbibliothek unterstützt wird. Beachte jedoch, dass die grundlegende Authentifizierung unverschlüsselt ist und deshalb für sensible Daten das HTTPS-Protokoll verwendet werden sollte.

## Weiterführende Informationen:

Du kannst mehr über das http-protokoll und die grundlegende Authentifizierung auf den folgenden Websites finden:

- [HTTP-Protokoll auf Wikipedia](https://de.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [Ruby 'net/http'-Dokumentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Verschiedene Arten von HTTP-Authentifizierung auf MDN](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)