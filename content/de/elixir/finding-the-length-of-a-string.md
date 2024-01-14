---
title:    "Elixir: Die Länge eines Strings finden"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

**Warum**: Es gibt viele Gründe, warum man sich beim Programmieren mit Elixir auseinandersetzen sollte. Einer davon ist die Fähigkeit, die Länge eines Strings zu finden. Dies ist eine grundlegende Fähigkeit, die in vielen Anwendungen benötigt wird, sei es zum Erstellen von Benutzeroberflächen oder zum Verarbeiten von Benutzereingaben. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies mit Elixir erreichen kann.

**Wie es geht**: Um die Länge eines Strings in Elixir zu finden, gibt es mehrere Möglichkeiten. Zunächst können wir die vordefinierte Funktion `length` verwenden. Diese Funktion nimmt einen String als Argument und gibt die Anzahl der Zeichen in dem String zurück.

```Elixir
IO.puts length("Hallo") # Ausgabe: 5
IO.inspect length("Ich bin ein Blog-Beitrag!") # Ausgabe: 25
```

Alternativ können wir auch die Funktion `String.length` verwenden, die das gleiche Ergebnis liefert.

```Elixir
IO.puts String.length("Elixir ist toll") # Ausgabe: 16
```

Wir können auch eine Schleife verwenden, um die Länge eines Strings zu finden, indem wir jedes Zeichen durchlaufen und die Anzahl der Schleifendurchläufe zählen.

```Elixir
def string_length(string) do
  count = 0

  for char <- String.graphemes(string) do
    count = count + 1
  end

  count
end

IO.puts string_length("Dies ist ein langer String") # Ausgabe: 25
```

**Tiefergehende Erklärung**: Wenn wir uns die Funktion `String.length` genauer ansehen, sehen wir, dass sie intern die Funktion `String.graphemes` verwendet, um den String in einzelne Zeichen zu zerlegen. Dies ist wichtig, da in Elixir Strings nicht als einzelne Zeichen, sondern als Liste von UTF-8-Codes dargestellt werden. Daher ist es wichtig, diese Liste zuerst zu zerlegen, um die tatsächliche Anzahl der Zeichen zu erhalten.

Es ist auch wichtig zu beachten, dass die Länge eines Strings nicht unbedingt mit der Anzahl der Buchstaben übereinstimmen muss. Einige Sprachen haben Zeichen, die aus mehreren Bytes bestehen, z.B. einige arabische oder chinesische Schriftzeichen. Dies kann zu Verwirrung führen, insbesondere wenn man die Länge eines Strings für die Überprüfung von Eingaben verwendet. In solchen Fällen ist es wichtig, die UTF-8-Codierung zu verstehen und die richtigen Funktionen für die Längenberechnung zu verwenden.

**Siehe auch**: 
- [Elixir Documentation - String Module](https://hexdocs.pm/elixir/String.html)
- [String-Related Functions in Elixir](https://www.culttt.com/2015/08/26/string-related-functions-in-elixir/)
- [Understanding UTF-8 and Unicode in Elixir](https://www.djm.org.uk/posts/understanding-utf-8-and-unicode-in-elixir/)