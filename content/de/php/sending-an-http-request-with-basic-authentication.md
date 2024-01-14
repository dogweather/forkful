---
title:                "PHP: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Das Senden von HTTP-Anfragen mit Basiskennung (basic authentication) ist eine grundlegende Fähigkeit für jeden PHP-Entwickler. Mit diesem Verfahren können Sie Ihre Anwendungen vor unerwünschtem Zugriff schützen und sicherstellen, dass nur berechtigte Nutzer Zugriff auf bestimmte Ressourcen haben.

# Wie man es macht

Die Verwendung von Basiskennung in PHP ist relativ einfach. Sie müssen lediglich den in PHP integrierten CURL (Client URL Library) verwenden. Hier ist ein Beispielcode:

```PHP
<?php
//URL der API
$url = "http://www.example.com/api/users";

//Benutzername und Passwort für die Basiskennung
$username = "username";
$password = "password";

//CURL-Resource erstellen
$curl = curl_init();

//Einstellungen für die HTTP-Anfrage
curl_setopt($curl, CURLOPT_URL, $url);
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

//Einstellungen für die Basiskennung
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "$username:$password");

//HTTP-Anfrage ausführen
$response = curl_exec($curl);

//Ausgabe der Ergebnisse
echo "Server-Antwort: " . $response;

//CURL ressource schließen
curl_close($curl);

?>
```

Das obige Beispiel zeigt wie Sie eine einfache HTTP-Anfrage mit Basiskennung in PHP durchführen können. Sie müssen lediglich die URL, den Benutzernamen und das Passwort angeben und die CURL-Einstellungen entsprechend setzen. Danach können Sie die Antwort des Servers ausgeben und die CURL-Resource schließen.

# Tieferer Einblick

Bei Basiskennung handelt es sich um eine grundlegende Authentifizierungsmethode im HTTP-Protokoll. Dabei werden Benutzerdaten in Form von Benutzername und Passwort mithilfe von Base64-Verschlüsselung übertragen. Dies ist zwar nicht die sicherste Methode, da die Daten im Klartext übertragen werden, ist jedoch ausreichend für grundlegende Sicherheitsanforderungen.

Sie können auch zusätzliche Optionen in CURL setzen, um die Sicherheit zu erhöhen, z.B. die Verwendung von HTTPS statt HTTP oder die Verwendung von mehrstufiger (digest) Authentifizierung. Es empfiehlt sich auch, Benutzerdaten in einer sicheren Datenbank zu speichern und nicht direkt in den PHP-Code einzuschließen.

# Siehe auch

- [PHP CURL Dokumentation](https://www.php.net/manual/en/book.curl.php)
- [HTTP Basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme) (englisch)
- [HTTP hierarchical authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#hierarchical_authentication_scheme) (englisch)