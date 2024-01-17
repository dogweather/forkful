---
title:                "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
html_title:           "Java: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung bedeutet, dass der Benutzername und das Passwort als Teil einer HTTP-Anfrage gesendet werden, um auf eine geschützte Ressource zuzugreifen. Programmierer tun dies, um sicherzustellen, dass nur autorisierte Benutzer Zugang zu sensiblen Daten oder Funktionen haben.

## Wie geht's:

```Java
// Bevor wir eine Anfrage senden können, müssen wir ein URL-Objekt erstellen
URL url = new URL("https://www.beispiel.com/geschützte-ressource");

// Dann erstellen wir die Verbindung und öffnen einen OutputStream, um Daten zu senden
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setDoOutput(true);
OutputStream os = con.getOutputStream();

// Jetzt setzen wir die grundlegende Authentifizierung für unsere Anfrage
String username = "meinBenutzername";
String password = "meinPasswort";
String credentials = username + ":" + password;
String encodedCredentials = Base64.getEncoder().encodeToString(credentials.getBytes());
con.setRequestProperty("Authorization", "Basic " + encodedCredentials);

// Wir können nun unsere Anfrage senden und die Antwort erhalten
os.write("Dies ist der Körper der Anfrage".getBytes());
InputStream is = con.getInputStream();

// Wir können die Antwort auslesen und anzeigen
InputStreamReader reader = new InputStreamReader(is);
BufferedReader in = new BufferedReader(reader);
String inputLine;
while((inputLine = in.readLine()) != null) {
    System.out.println(inputLine);
}

// Vergessen Sie nicht, die Verbindung zu schließen und die Streams zu leeren
in.close();
os.close();
con.disconnect();
```

## Tief tauchen:

Nicht immer ist es sicher, Benutzername und Passwort als Teil einer HTTP-Anfrage zu senden. Alternativ können digitale Zertifikate verwendet werden, um die Authentifizierung zu ermöglichen. Die Implementierung der grundlegenden Authentifizierung variiert auch je nach Programmiersprache und Framework.

## Siehe auch:

- https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
- https://docs.oracle.com/javase/tutorial/networking/urls/authenticate.html