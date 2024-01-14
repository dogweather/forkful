---
title:    "Elixir: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum
Manchmal kann es notwendig sein, einen String in Kleinbuchstaben umzuwandeln, zum Beispiel wenn man Benutzereingaben vergleichen oder formatieren muss. In diesem Blogpost zeige ich Ihnen, wie Sie dies in Elixir ganz einfach erreichen können.

# Wie geht das?
Das Umwandeln von Strings in Kleinbuchstaben ist in Elixir sehr einfach. Dazu können Sie die Funktion `String.downcase/1` verwenden. Hier ein Beispiel:

```elixir
String.downcase("Hallo Welt")
```
Das obige Beispiel würde den String "Hallo Welt" in "hallo welt" umwandeln. Sie können auch Variablen anstatt von festen Werten verwenden, zum Beispiel:

```elixir
text = "DIESER TEXT SOLL IN KLEINBUCHSTABEN UMGEWANDELT WERDEN"
String.downcase(text)
```
Dieses Beispiel würde den Inhalt der Variable `text` in Kleinbuchstaben umwandeln.

# Tiefergehende Informationen
Es ist wichtig zu beachten, dass die `String.downcase/1` Funktion nicht den ursprünglichen String ändert, sondern einen neuen String in Kleinbuchstaben zurückgibt. Der ursprüngliche String bleibt unverändert. Wenn Sie also den neuen String speichern möchten, müssen Sie ihn einer Variable zuweisen, wie im Beispiel oben.

Es gibt auch andere Möglichkeiten, Strings in Kleinbuchstaben umzuwandeln, zum Beispiel die Verwendung von Mustern und regulären Ausdrücken. Wenn Sie tiefer in das Thema einsteigen möchten, empfehle ich Ihnen, die offizielle Dokumentation zu diesem Thema zu lesen: [https://hexdocs.pm/elixir/String.html#downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1).

# Siehe auch
- [https://hexdocs.pm/elixir/String.html#downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [https://stackoverflow.com/questions/36734696/how-to-convert-a-string-to-upper-case-or-lower-case-in-elixir](https://stackoverflow.com/questions/36734696/how-to-convert-a-string-to-upper-case-or-lower-case-in-elixir)
- [https://elixirschool.com/en/lessons/basics/string/](https://elixirschool.com/en/lessons/basics/string/)