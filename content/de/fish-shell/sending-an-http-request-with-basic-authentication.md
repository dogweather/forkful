---
title:                "Versenden einer http-Anforderung mit grundlegender Authentifizierung"
html_title:           "Fish Shell: Versenden einer http-Anforderung mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anforderung mit grundlegender Authentifizierung"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden von HTTP-Anfragen mit einer einfachen Authentifizierung ist ein gängiges Verfahren in der Programmierung, um Zugriff auf geschützte Ressourcen zu erhalten. Es wird häufig verwendet, um APIs, Webdienste oder andere Online-Plattformen anzusteuern, die einen Benutzernamen und ein Passwort zur Authentifizierung erfordern.

## Wie?

Die Verwendung von Fish Shell vereinfacht das Senden von HTTP-Anfragen mit einfacher Authentifizierung erheblich. Mit dem Befehl "curl" können wir eine Anfrage senden und dabei die Option "-u" verwenden, um die Anmeldeinformationen anzugeben. Hier ist ein Beispiel:

```
Fish Shell: curl -u Benutzername:Passwort https://example.com/api/
```

Das gibt uns den vollen Inhalt der angeforderten Ressource als Ausgabe zurück. Wenn Sie nur eine bestimmte Eigenschaft oder einen Wert aus der Antwort benötigen, können Sie die Option "-s" verwenden, um sie zu filtern, zum Beispiel:

```
Fish Shell: curl -su Benutzername:Passwort https://example.com/api/ | grep "E-Mail"
```

Dies wird nur die Zeile mit der E-Mail-Adresse des Benutzers aus der Antwort ausgeben.

## Tiefer Einblick

Die Verwendung von HTTP-Anfragen mit einfacher Authentifizierung hat eine lange Geschichte und ist immer noch eines der grundlegendsten Verfahren zur Authentifizierung in der Programmierung. Es gibt jedoch auch andere Methoden, wie beispielsweise OAuth 2.0 oder Token-basierte Authentifizierung, die je nach Anwendungsfall möglicherweise geeigneter sind.

Wenn Sie mehr über das Senden von HTTP-Anfragen mit Fish Shell erfahren möchten, können Sie die Manpage für "curl" oder die offizielle Dokumentation für Fish Shell konsultieren. Es gibt auch verschiedene Plug-ins und Erweiterungen, die das Senden von HTTP-Anfragen noch einfacher machen und Funktionen wie das Caching von Anmeldeinformationen oder das Automatisieren von Anfragen ermöglichen.

## Siehe auch

- Fish Shell Dokumentation: https://fishshell.com/docs/current/
- Offizielle Manpage für "curl": https://curl.haxx.se/docs/manpage.html
- Erweiterungen für Fish Shell: https://github.com/fisherman/fisherman/wiki/Plugins