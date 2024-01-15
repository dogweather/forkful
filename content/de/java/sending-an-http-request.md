---
title:                "Versenden einer http-Anfrage"
html_title:           "Java: Versenden einer http-Anfrage"
simple_title:         "Versenden einer http-Anfrage"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand eine HTTP-Anfrage senden? Nun, es gibt viele Gründe dafür. Zum Beispiel könnte man Daten von einer externen API abrufen, eine Webseite für Benutzerdaten authentifizieren oder einfach nur einen einfachen GET-Anruf für die Datenabfrage durchführen.

## Wie geht es

Um eine HTTP-Anfrage in Java zu senden, gibt es ein paar Schritte zu beachten. Zunächst müssen Sie die `HttpURLConnection`-Klasse importieren, um eine Verbindung zu einer URL aufzubauen. Dann müssen Sie eine URL-Instanz mit der entsprechenden URL erstellen, zu der Sie die Anfrage senden möchten.

```
import java.net.HttpURLConnection;
import java.net.URL;

URL url = new URL("https://www.example.com/api/data"); // Erstellen Sie eine URL-Instanz
HttpURLConnection connection = (HttpURLConnection) url.openConnection(); // Öffnen Sie eine Verbindung
```

Als nächstes müssen Sie die HTTP-Methode und die Anfrageeigenschaften festlegen.

```
connection.setRequestMethod("GET"); // Setzen Sie die HTTP-Methode
connection.setConnectTimeout(5000); // Setzen Sie eine Verbindungszeitüberschreitung (optional)
connection.setReadTimeout(5000); // Setzen Sie eine Lesezeitüberschreitung (optional)
```

Wenn Sie Daten an die Anfrage anhängen müssen, können Sie dies mit der `setDoOutput(true)`-Methode tun und dann die `getOutputStream()`-Methode verwenden, um die Daten an die Anfrage zu schreiben.

```
connection.setDoOutput(true); // Sagen Sie der Verbindung, dass Sie Daten senden möchten
OutputStream os = connection.getOutputStream();
os.write("name=Max".getBytes());
```

Schließlich können Sie die Anfrage senden und die Antwort erhalten.

```
int responseCode = connection.getResponseCode(); // Holen Sie den Antwortcode
InputStream inputStream = connection.getInputStream(); // Holen Sie die Antwortdaten

StringBuffer response = new StringBuffer();
try (BufferedReader in = new BufferedReader(new InputStreamReader(inputStream))) {
    String line;
    while ((line = in.readLine()) != null) {
        response.append(line);
    }
}

System.out.println(response.toString()); // Geben Sie die Antwort aus
```

## Tiefer gehen

Jetzt haben Sie eine Grundidee, wie Sie eine HTTP-Anfrage in Java senden können. Sie können auch Parameter zu Ihrer Anfrage hinzufügen, indem Sie die `setRequestProperty()`-Methode verwenden. Sie können auch die `setRequestMethod()` verwenden, um verschiedene HTTP-Methoden wie PUT, POST oder DELETE zu verwenden.

Wenn Sie tiefer in die Materie gehen möchten, können Sie sich auch mit den verschiedenen HTTP-Statuscodes beschäftigen und wie Sie auf diese in Ihrem Code reagieren können.

## Siehe auch

- Offizielle Java-Dokumentation zu HttpURLConnection: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html
- Beispiel für das Senden einer HTTP-Anfrage in Java: https://www.baeldung.com/java-http-request