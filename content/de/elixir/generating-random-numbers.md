---
title:                "Elixir: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Elixir ist eine Programmiersprache, die auf der dynamischen Funktionssprache Erlang basiert. Sie bietet viele nützliche Funktionen, die es Entwicklern ermöglichen, effiziente und zuverlässige Anwendungen zu schreiben. Eine dieser Funktionen ist die Fähigkeit, Zufallszahlen zu generieren. Warum sollte man sich also mit der Generierung von Zufallszahlen in Elixir beschäftigen? Nun, Zufallszahlen sind von entscheidender Bedeutung in vielen Anwendungen, wie beispielsweise bei der Entwicklung von Spielen, dem Testen von Algorithmen oder dem Erstellen von Sicherheitstokens. In diesem Blog-Beitrag werden wir uns ansehen, wie man in Elixir Zufallszahlen generiert und einige interessante Anwendungen dafür erfahren.

## Wie geht das?

Um Zufallszahlen in Elixir zu generieren, verwenden wir die `:rand`-Funktion. Diese Funktion akzeptiert eine beliebige Anzahl von Argumenten und liefert eine Liste von Zufallszahlen zurück. Schauen wir uns ein Beispiel an:

```Elixir
:rand.uniform(100)  #=> 75
:rand.uniform(100)  #=> 43
```

In diesem Beispiel haben wir die Funktion `:rand.uniform/1` verwendet, um zufällige Zahlen zwischen 0 und 100 zu generieren. Jedes Mal, wenn wir die Funktion aufrufen, erhalten wir eine andere Zufallszahl zurück. Wir können auch einen Bereich angeben, aus dem die Zufallszahlen generiert werden sollen, indem wir ein Minimum und Maximum als Argumente übergeben:

```Elixir
:rand.uniform(50, 100)  #=> 82
:rand.uniform(50, 100)  #=> 53
```

Um eine Liste von Zufallszahlen zu erhalten, können wir die Funktion `:rand.uniform/2` mehrmals aufrufen und die erzeugten Zahlen in eine Liste einfügen:

```Elixir
list = [:rand.uniform(100), :rand.uniform(100), :rand.uniform(100)]
#=> [71, 14, 96]
```

Alternativ können wir auch die Hilfsfunktion `:rand.seed/1` verwenden, um die Generierung von Zufallszahlen mit einer bestimmten Startnummer zu beginnen. Dies ist besonders nützlich, wenn wir möchten, dass unsere Zufallszahlen in verschiedenen Iterationen eines Programms konsistent bleiben.

```Elixir
:rand.seed(1234)
list = [:rand.uniform(100), :rand.uniform(100), :rand.uniform(100)]
#=> [88, 13, 57]
```

Jetzt, wo wir wissen, wie man Zufallszahlen in Elixir generiert, schauen wir uns genauer an, wie diese Funktion unter der Haube funktioniert.

## Tiefergehende Informationen

Elixir verwendet eine Pseudozufallszahlengenerator (PRNG), um Zufallszahlen zu generieren. Dieser Generator berechnet Zufallszahlen anhand eines Anfangswerts, der als "Seed" bezeichnet wird. Dieser Seed wird dann verwendet, um eine Folge von Zahlen zu generieren, die als zufällig erscheinen. Der interessante Teil ist, dass wir die gleiche Folge von Zahlen erhalten, wenn wir denselben Seed und dieselbe Anzahl von Aufrufen verwenden. Dies ist der Grund, warum wir den Seed mit der Funktion `:rand.seed/1` setzen können, um konsistente Zufallszahlen zu erhalten. Eine wichtige Sache zu beachten ist jedoch, dass die Generierung von Zufallszahlen nur so gut ist wie der PRNG, der verwendet wird. In der Praxis verwendet Elixir einen PRNG-Algorithmus namens "Mersenne Twister", der als ausreichend zufällig und effizient angesehen wird.

## Siehe auch

- Offizielle Elixir Dokumentation: https://hexdocs.pm/elixir/rand.html
- Artikel über den Mersenne Twister Algorithmus: https://en.wikipedia.org/wiki/Mersenne_Twister
- Elixir Forum Diskussion über das Setzen von Seeds: https://elixirforum.com/t/what-happens-when-setting-seeds-to-generate-random-numbers/36624