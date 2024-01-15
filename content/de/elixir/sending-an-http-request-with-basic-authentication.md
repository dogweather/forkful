---
title:                "Senden einer http-Anforderung mit grundlegender Authentifizierung"
html_title:           "Elixir: Senden einer http-Anforderung mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anforderung mit grundlegender Authentifizierung"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Menschen senden HTTP-Anfragen mit grundlegender Authentifizierung, um sich bei einem Webdienst anzumelden oder eine Aktion auszuführen, die eine identifizierte Identität erfordert. Dies ist in vielen Fällen erforderlich, um auf geschützte Ressourcen zuzugreifen oder vertrauliche Daten zu übertragen.

## Wie geht das

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Elixir zu senden, müssen Sie das HTTPoison-Paket installieren und verwenden. Im folgenden Codeblock sehen Sie ein Beispiel für eine GET-Anfrage mit grundlegender Authentifizierung:

```Elixir
{:ok, response} = HTTPoison.get(
  "https://www.example.com",
  headers: [
    {"Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ="}       
  ]
)
```

Der erste Parameter ist die URL, zu der die Anfrage gesendet wird. Im zweiten Parameter fügen Sie die erforderlichen Header hinzu, wobei der Authorization-Header die Benutzername und das Passwort in Base64-codiertem Format enthält (in diesem Beispiel ist der Benutzername "username" und das Passwort "password"). Der `response` enthält die Antwort des Servers, die Sie dann weiterverarbeiten können.

## Tief tauchen

Bei der grundlegenden Authentifizierung werden die Benutzeranmeldeinformationen in einem Base64-codierten String an den Server gesendet. Der Server entschlüsselt diesen String und authentifiziert den Benutzer auf Basis der bereitgestellten Informationen. Es ist wichtig zu beachten, dass diese Methode NICHT sicher ist, da der Base64-codierte String relativ einfach zu entschlüsseln ist. Für eine sicherere Authentifizierung sollten Sie eine andere Methode, wie z.B. OAuth, verwenden.

## Siehe auch

- [HTTPoison Dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [HTTP Grundlagen](https://www.tutorialspoint.com/http/http_overview.htm)
- [Elixir Programmiersprache](https://elixir-lang.org/)