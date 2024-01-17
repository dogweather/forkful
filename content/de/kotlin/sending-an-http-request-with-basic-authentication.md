---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Kotlin: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist eine Möglichkeit für Programmierer, sicher auf bestimmte Dienste oder Ressourcen zuzugreifen. Dabei werden Benutzername und Passwort verwendet, um die Identität des Nutzers zu überprüfen und ihm Zugriff auf geschützte Inhalte zu gewähren.

# Wie geht das?

Kotlin bietet eine einfache, effiziente Möglichkeit, HTTP-Anfragen mit grundlegender Authentifizierung zu senden. Folgendes Codebeispiel zeigt, wie man mithilfe der "HTTP Client"-Klasse von Kotlin eine GET-Anfrage mit grundlegender Authentifizierung senden kann:

```Kotlin
HttpClient().get<String>("http://example.com") {
    auth {
        username = "Benutzername"
        password = "Passwort"
    }
}
```

Das Ergebnis der Anfrage wird als String zurückgegeben. Hier ist ein Beispiel für eine erfolgreiche Anfrage mit dem Statuscode "200":

```Kotlin
HTTP/1.1 200 OK
```

# Tiefere Einblicke

Die grundlegende Authentifizierung ist eine der ältesten Methoden zur Sicherung von Webanwendungen und ist seit den Anfängen des World Wide Web im Einsatz. Es gibt auch Alternative Methoden wie die Digest- und OAuth-Authentifizierung. 

Bei der Implementierung von grundlegender Authentifizierung wird der Benutzername und das Passwort Base64-kodiert und als Teil des Authorization-Headers der HTTP-Anfrage übertragen. Um die Sicherheit zu erhöhen, sollte jedoch zusätzlich eine SSL-Verschlüsselung eingesetzt werden.

# Siehe auch

Weitere Informationen zu HTTP-Anfragen mit grundlegender Authentifizierung und Kotlin finden Sie in der offiziellen Dokumentation von Kotlin: https://kotlinlang.org/docs/reference/coroutines/coroutines-guide.html#http-client