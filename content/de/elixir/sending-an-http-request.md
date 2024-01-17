---
title:                "Eine http-Anfrage senden"
html_title:           "Elixir: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Senden einer HTTP-Anfrage ist ein wichtiger Prozess im Bereich der Webentwicklung. Es ermöglicht Programmierern, Daten von einer Website oder einem Webdienst zu erhalten oder zu senden. Dies ist hilfreich für die Erstellung interaktiver Webanwendungen oder für die Einbindung von externen Diensten in eine Anwendung.

So geht's:
Elixir macht das Senden von HTTP-Anfragen einfach mit der in der Standardbibliothek enthaltenen HTTPoison-Bibliothek. Hier ist ein Beispiel, um eine GET-Anfrage an eine URL zu senden und dann die JSON-Antwort zu verarbeiten:

```
Elixir def send_http_request(url) do
  response = HTTPoison.get(url)
  case response do
    {:ok, %{status_code: 200, body: body}} -> IO.puts("Antwort: #{body}")
    _ -> IO.puts("Es gab ein Problem bei der Anfrage")
  end
end
```

Die Ausgabe wird je nach Antwort der URL variieren. Wenn die Anfrage erfolgreich ist, wird der Inhalt der Antwort ausgegeben. Andernfalls wird eine Fehlermeldung ausgegeben.

Vertiefung:
Das HTTP-Protokoll wurde in den 1990er Jahren von Tim Berners-Lee entwickelt und ist ein grundlegender Teil des World Wide Web. Es gibt auch alternative Möglichkeiten, HTTP-Anfragen zu senden, wie z.B. die in Elixir ebenfalls verfügbare :ibrowse-Bibliothek oder das populäre cURL-Tool.

Weiterführende Links:
- Offizielle Elixir-Dokumentation zur HTTPoison-Bibliothek: https://hexdocs.pm/httpoison/
- Weitere Möglichkeiten, HTTP-Anfragen in Elixir zu senden: https://hexdocs.pm/elixir/master/elixir-libraries.html#http-clients