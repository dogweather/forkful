---
title:                "Elixir: Senden einer Http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer Http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Die Übermittlung von HTTP-Anfragen mit grundlegender Authentifizierung ist ein wichtiger Teil der modernen Webentwicklung. Es ermöglicht es einer Anwendung, sich bei einem Server zu authentifizieren und erweiterte Berechtigungen für bestimmte Aktionen zu erhalten.

## How To

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Elixir zu senden, können wir das `HTTPoison`-Paket verwenden. Dieses Paket bietet eine einfache API zum Senden von HTTP-Anfragen und Unterstützung für grundlegende Authentifizierung.

```elixir
# Installation des HTTPoison-Pakets
{:httpoison, "~> 1.6"}

# Senden einer HTTP-Anfrage mit grundlegender Authentifizierung
HTTPoison.get("https://www.example.com", [], auth: {"username", "password"})
```

Die `Auth`-Option erlaubt es uns, Benutzername und Passwort für die grundlegende Authentifizierung anzugeben. Wir können auch andere Optionen wie Header oder Body der Anfrage hinzufügen, indem wir sie als Schlüssel-Wert-Paare in einem Tupel angeben.

## Deep Dive

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung beinhaltet die Übermittlung von Benutzername und Passwort im Header der Anfrage. Dies geschieht in Form von Base64-kodierten Daten, die im Format `Benutzername:Passwort` angegeben werden. Diese Daten werden dann vom Server überprüft, um zu bestätigen, dass der Benutzer gültige Anmeldeinformationen hat.

Ein wichtiger Punkt zu beachten ist, dass grundlegende Authentifizierung keine sichere Methode ist, um Benutzer zu authentifizieren. Die Übermittlung von Anmeldedaten über HTTP ist anfällig für Abhören und sollte daher nur verwendet werden, wenn zusätzliche Sicherheitsmaßnahmen wie Transport Layer Security (TLS) implementiert werden.

## Siehe auch

- Offizielle Dokumentation von HTTPoison: https://hexdocs.pm/httpoison/1.6.0/
- Eine Einführung in Elixir-Webentwicklung: https://blog.codeship.com/an-introduction-to-elixir-web-development/