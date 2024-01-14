---
title:                "Java: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Warum herunterladen Sie eine Webseite?

Das Herunterladen von Webseiten ist eine häufige Aufgabe für Programmierer, insbesondere wenn sie im Bereich Webentwicklung tätig sind. Oftmals müssen bestimmte Teile der Webseite analysiert oder verändert werden, was ohne das Herunterladen der Seite nicht möglich wäre. Außerdem ist es eine nützliche Fähigkeit, die dazu beitragen kann, tiefer in die Welt des Webdesigns einzutauchen.

Wie man eine Webseite herunterlädt

Um eine Webseite herunterzuladen, können Sie die Klasse "URLConnection" aus dem java.net Paket verwenden. Mit dieser Klasse können Sie eine Verbindung zu einer URL aufbauen und den Inhalt der Webseite abrufen.

```
java.net.URL url = new java.net.URL("http://www.example.com");
java.net.URLConnection conn = url.openConnection();
java.io.InputStream in = conn.getInputStream();
java.io.FileOutputStream out = new java.io.FileOutputStream("webseite.html");
byte[] buffer = new byte[1024];
int bytesRead;
while((bytesRead = in.read(buffer)) != -1){
    out.write(buffer, 0, bytesRead);
}
in.close();
out.close();
```
Diese einfache Implementierung erstellt eine Datei mit dem Namen "webseite.html", die den Inhalt der Webseite enthält.

Tiefergehen

Es gibt verschiedene Möglichkeiten, wie Sie das Herunterladen einer Webseite mit Java anpassen können. Zum Beispiel können Sie die "URLConnection" Klasse verwenden, um einen benutzerdefinierten Benutzer-Agenten zu erstellen, um sich als Desktop-Browser auszugeben und somit die Ergebnisse Ihrer Anfrage zu optimieren. Sie können auch verschiedene Arten von Anforderungen senden, wie zum Beispiel POST oder PUT Anforderungen. Die Möglichkeiten sind endlos und können je nach Ihren spezifischen Anforderungen angepasst werden.

Siehe auch

1. https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html
2. https://docs.oracle.com/javase/10/docs/api/java/net/URLConnection.html
3. https://www.baeldung.com/java-http-url-connection