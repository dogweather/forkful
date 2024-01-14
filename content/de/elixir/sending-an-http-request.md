---
title:                "Elixir: Eine http Anfrage senden"
simple_title:         "Eine http Anfrage senden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Es ist wichtig, dass Entwickler die Grundlagen des Sendens von HTTP-Anfragen in Elixir verstehen, da dies ein wichtiger Bestandteil der Interaktion mit Webanwendungen und APIs ist. HTTP-Anfragen ermöglichen es uns, Daten von anderen Servern zu empfangen und zu verarbeiten, was für die Erstellung dynamischer Anwendungen unerlässlich ist.

## Wie man es macht

Die Verwendung von HTTP-Anfragen in Elixir ist relativ einfach. Wir können das `HTTPoison`-Paket verwenden, um eine Anfrage an eine URL zu senden und die Antwort zu verarbeiten. Hier ist ein Beispielcode:

```elixir
# Stellt sicher, dass das HTTPoison-Paket installiert ist
defp deps do
  [{:httpoison, "~> 1.5"}]
end

# Führt eine GET-Anfrage an eine URL aus und gibt die Antwort zurück
response = HTTPoison.get("https://api.example.com/endpoint")

# Verarbeitet die Antwort und gibt die Daten aus
case response do
  {:ok, response} ->
    # Konvertiert die Daten in ein JSON-Format, falls erforderlich
    json_response = Poison.Parser.parse!(response.body)
    # Verarbeitet die JSON-Daten und gibt sie aus
    IO.puts("Antwort erhalten: #{json_response["data"]}")
  {:error, error} ->
    # Gibt eine Fehlermeldung aus, falls die Anfrage nicht erfolgreich war
    IO.inspect error
end
```

Das oben genannte Beispiel zeigt eine grundlegende GET-Anfrage, aber wir können auch andere Anfragemethoden wie POST, PUT, DELETE usw. verwenden, indem wir den entsprechenden HTTP-Verb in der `HTTPoison`-Funktion angeben. Sie können auch weitere Optionen wie Headers, Parameter und Cookies hinzufügen, je nach Anforderungen Ihrer Anwendung.

## Tiefergehende Informationen

Es gibt viele Möglichkeiten, die `HTTPoison`-Bibliothek an Ihre Bedürfnisse anzupassen und zu konfigurieren. Sie können beispielsweise Caching-Funktionen hinzufügen, um die Leistung zu verbessern, oder Websocket-Verbindungen für Echtzeitkommunikation aufbauen. Es ist auch wichtig zu verstehen, wie man mit Fehlern umgeht und wie man die Anfragegeschwindigkeit optimiert.

## Siehe auch

- Offizielle Dokumentation von HTTPoison - https://hexdocs.pm/httpoison/
- Elixir-Skripte mit HTTP-Anfragen - https://thoughtbot.com/blog/making-http-requests-in-elixir-with-httpoison
- Einführung in die Verwendung von HTTP-Anfragen in Elixir - https://medium.com/better-programming/sending-requests-with-elixir-b0c463994178