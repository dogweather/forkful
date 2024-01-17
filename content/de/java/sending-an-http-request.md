---
title:                "Eine http-Anfrage senden"
html_title:           "Java: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage ist ein wichtiger Teil der Programmierung in Java. Durch das Senden von HTTP-Anfragen können Programmierer mit anderen Programmen, Servern oder APIs kommunizieren und Daten austauschen. Es ist ein wesentlicher Schritt in der Entwicklung von webbasierten Anwendungen oder Web Services.

## Wie geht's?
Um eine HTTP-Anfrage in Java zu senden, muss die Klasse "HttpURLConnection" verwendet werden. Hier ist ein Beispielcode, der eine GET-Anfrage an eine URL sendet:

```Java
try {
  URL url = new URL("https://www.example.com");
  HttpURLConnection con = (HttpURLConnection) url.openConnection();
  con.setRequestMethod("GET");
  int responseCode = con.getResponseCode();
  System.out.println("Response Code: " + responseCode);
} catch (IOException e) {
  e.printStackTrace();
}
```

Das obige Beispiel verwendet die Methode "getResponseCode()" um den Statuscode der Antwort zu erhalten. Weitere Informationen über die Antwort können durch andere Methoden wie "getHeaderField()" oder "getInputStream()" erhalten werden.

## Tiefgrabung
In der Vergangenheit war das Senden von HTTP-Anfragen in Java etwas komplizierter, da APIs wie "HttpURLConnection" oder "HttpClient" erst in späteren Versionen hinzugefügt wurden. Es gab jedoch alternative Frameworks oder Bibliotheken, die verwendet werden konnten, wie z.B. Apache Commons oder das JBoss HTTP-Client-Projekt.

In neueren Versionen von Java gibt es auch die Möglichkeit, mit der "java.net.http" API asynchrone HTTP-Anfragen zu senden. Dies kann bei der Entwicklung von performanten, asynchronen Anwendungen nützlich sein.

## Siehe auch
- [Official Java documentation for HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Apache Commons](https://commons.apache.org/)
- [JBoss HTTP-Client-Projekt](https://docs.jboss.org/author/display/AS71/HTTP+API+Guide)