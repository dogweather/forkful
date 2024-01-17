---
title:                "Ein http-Anfrage mit grundlegenden Authentifizierung senden"
html_title:           "Ruby: Ein http-Anfrage mit grundlegenden Authentifizierung senden"
simple_title:         "Ein http-Anfrage mit grundlegenden Authentifizierung senden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Grundauthentifizierung können verwendet werden, um geschützte Ressourcen auf einem Server abzurufen. Programmierer nutzen dies, um eine sichere Verbindung zu einem externen Server aufzubauen und Daten auszutauschen.

## Wie geht's:

Bevor man eine HTTP-Anfrage mit Grundauthentifizierung senden kann, muss man sich zuerst mit dem Server verbinden und die Authentifizierungsdaten angeben. Dies kann mit der Net::HTTP Bibliothek in Ruby geschehen. Hier ist ein Beispiel:

    require 'net/http'
    require 'uri'
    
    # Ziel-URL
    url = URI("http://www.example.com")
    
    # Verbindung herstellen
    http = Net::HTTP.new(url.host, url.port)
    
    # Authentifizierungsdaten angeben
    request = Net::HTTP::Get.new(url)
    request.basic_auth("username", "password")
    
    # Anfrage senden und Antwort erhalten
    response = http.request(request)
    puts response.body

Die `basic_auth` Methode wird verwendet, um die Authentifizierungsdaten in der Anfrage zu setzen. Wenn die Anfrage erfolgreich ist, wird die Antwort in der `response` Variable gespeichert und kann dann weiterverarbeitet werden.

## Tiefere Einblicke:
Grundauthentifizierung ist eine der ältesten Methoden zur Sicherung von HTTP-Anfragen. Es funktioniert, indem der Benutzername und das Passwort im Header der Anfrage im Base64 Format codiert werden. Dies kann jedoch leicht entschlüsselt werden und bietet daher keine ausreichende Sicherheit. Alternativen wie Digest-Authentifizierung oder die Verwendung von SSL sollten bevorzugt werden, wenn eine höhere Sicherheit erforderli