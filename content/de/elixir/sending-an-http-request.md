---
title:                "Das Senden einer http-Anfrage"
html_title:           "Elixir: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum
Wenn du mit Elixir programmierst, ist es sehr wahrscheinlich, dass du irgendwann eine HTTP-Anfrage senden musst. Das könnte zum Beispiel der Fall sein, wenn du mit einer externen API kommunizieren möchtest oder dein Programm Daten von einer Webseite abrufen muss. In diesem Artikel werden wir uns anschauen, wie man in Elixir eine HTTP-Anfrage sendet und was dabei zu beachten ist.

## Wie geht's
Um eine HTTP-Anfrage in Elixir zu senden, benötigen wir zuerst das Paket `HTTPoison`, das uns erlaubt, HTTP-Anfragen zu erstellen und zu versenden. Öffne dein Projekt in einem Code-Editor und füge folgende Zeile zur `mix.exs` Datei hinzu:

```Elixir
defp deps do
  [{:httpoison, "~> 1.0"}]
end
```

So haben wir `HTTPoison` zu unserem Projekt hinzugefügt. Jetzt öffne die `mix.exs` Datei im Terminal und führe `mix deps.get` aus, um das Paket herunterzuladen und zu installieren.

Als nächstes müssen wir das Paket in unserer Datei importieren, in der wir die HTTP-Anfrage senden wollen. Füge dazu folgende Zeile am Anfang deiner Datei hinzu:

```Elixir
import HTTPoison
```

Jetzt können wir die Funktion `HTTPoison.get()` verwenden, um eine Anfrage zu senden. Hier ist ein Beispiel, wie man Daten von der Google-API abruft:

```Elixir
res = HTTPoison.get("https://www.googleapis.com/books/v1/volumes?q=elixir")
```

Hier senden wir eine GET-Anfrage an die Google-API und übergeben als Parameter den Suchbegriff "elixir". Das Ergebnis wird in der Variablen `res` gespeichert. Um die Antwort zu überprüfen, können wir einfach `res.body` aufrufen, um den Inhalt der Antwort zu sehen.

## Deep Dive
Wenn wir eine HTTP-Anfrage senden, gibt es ein paar Dinge, die wir beachten müssen. Zum Beispiel müssen wir die HTTPS-Verbindung überprüfen, um sicherzustellen, dass unsere Anfrage sicher ist. Dazu können wir die Funktion `HTTPS.verify()` verwenden, die vom `HTTPoison` Paket bereitgestellt wird.

```Elixir
res = HTTPoison.get("https://www.googleapis.com/books/v1/volumes?q=elixir", [:verify_peer])
```

Hier übergeben wir ein zusätzliches Argument `[:verify_peer]` an die `HTTPoison.get()` Funktion, um die Verbindung zu überprüfen.

Außerdem können wir auch Header und Body-Parameter zu unserer Anfrage hinzufügen, falls wir diese benötigen. Dafür gibt es in `HTTPoison` spezielle Funktionen wie `HTTPoison.post()` oder `HTTPoison.put()`, die es uns erlauben, POST- oder PUT-Anfragen zu erstellen.

Eine wichtige Sache, die man im Hinterkopf behalten sollte, ist, dass Funktionen von `HTTPoison` wie `get()` oder `post()` asynchron sind, d.h. sie führen die Anfrage im Hintergrund aus und geben direkt ein Versprechen (Promise) zurück. Um die tatsächliche Antwort zu erhalten, müssen wir die Funktion `HTTPoison.await/2` nutzen, die das Versprechen in ein konkretes Ergebnis umwandelt.

```Elixir
{:ok, status, body} = HTTPoison.await(HTTPoison.get("https://www.googleapis.com/books/v1/volumes?q=elixir", [:verify_peer]))
```

Hier rufen wir `HTTPoison.get()` auf und warten auf das Ergebnis, bevor wir es in die Variablen `status` und `body` speichern.

## Siehe auch
- Offizielle Dokumentation von `HTTPoison`: https://hexdocs.pm/httpoison/readme.html
- Einführung in das Senden von HTTP-Anfragen mit Elixir: https://medium.com/@kahlil/how-to-send-http-requests-in-elixir-solving-the-stickiest-problem-in-functional-programming-22649ca66657