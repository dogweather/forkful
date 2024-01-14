---
title:                "Elixir: Auslesen von Unterzeichenketten"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist eine häufig verwendete Funktion in der Programmierung. Es ermöglicht uns, Teilstrings aus einem string zu extrahieren, die wir für spätere Berechnungen, Überprüfungen oder Anzeigen benötigen. In Elixir ist die substring-Funktion sehr effizient und einfach zu verwenden, was sie zu einem wertvollen Werkzeug in unserem Programmierarsenal macht.

## Wie man es macht

Um eine Substring aus einem Elixir string zu extrahieren, verwenden wir die `String.slice/3` Funktion. Diese Funktion nimmt drei Argumente an: der string, von dem wir eine Teilstring extrahieren möchten, der Anfangsindex und der Endindex. Wenn wir beispielsweise den string `"Elixir ist großartig"` haben und den Teilstring `"großartig"` extrahieren möchten, können wir folgenden Code schreiben:

```Elixir
String.slice("Elixir ist großartig", 11, 19)
```

Das Ergebnis wäre `"großartig"`, da der Anfangsindex 11 der Buchstabe "g" ist und der Endindex 19 das "i" am Ende des Wortes ist. Beachten Sie, dass der Endindex nicht Teil des resultierenden Teilstrings ist.

Wir können auch negative Indizes verwenden, um von hinten zu zählen. Zum Beispiel würde `String.slice("Elixir ist großartig", -9, -1)` den Teilstring `"Elixir"` extrahieren.

Wenn wir einen Teilstring bis zum Ende des string extrahieren möchten, können wir den `String.length/1` Operator verwenden, um die Länge des strings zu erhalten. Zum Beispiel würde `String.slice("Elixir ist großartig", 0, String.length("Elixir ist großartig"))` den gesamten string zurückgeben.

## Tief eintauchen

Neben der `String.slice/3` Funktion, gibt es in Elixir auch die `String.split/2` Funktion, die uns ermöglicht, einen string in bestimmten Abschnitten aufzuteilen. Wir können einen Teilstring als Trennzeichen verwenden und erhalten dann eine Liste der aufgeteilten Abschnitte. Zum Beispiel würde `String.split("Elixir::ist::großartig", "::")` die Liste `["Elixir", "ist", "großartig"]` zurückgeben.

Eine andere nützliche Funktion ist `String.trim/1`, die uns ermöglicht, Leerzeichen oder Zeilenumbrüche am Anfang oder Ende eines strings zu entfernen. Dies ist besonders hilfreich, wenn wir mit Benutzereingaben arbeiten.

## Siehe auch

- [Elixir-Dokumentation zu Strings](https://hexdocs.pm/elixir/String.html)
- [Elixir School: Strings](https://elixirschool.com/de/lessons/basics/string/)
- [ElixirForum: Substring extrahieren](https://elixirforum.com/t/getting-a-substring/1102)