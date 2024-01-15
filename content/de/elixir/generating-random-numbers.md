---
title:                "Generieren von Zufallszahlen"
html_title:           "Elixir: Generieren von Zufallszahlen"
simple_title:         "Generieren von Zufallszahlen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Erzeugung von Zufallszahlen beschäftigen? Nun, es gibt viele Anwendungen, bei denen zufällige Daten benötigt werden, wie zum Beispiel in der Kryptographie, bei Computerspielen oder in wissenschaftlichen Simulationen. Die Elixir-Programmiersprache bietet robuste und effiziente Methoden zur Erzeugung von Zufallszahlen, die in diesem Artikel erkundet werden sollen.

## How To

Die Funktion `:random.uniform/2` wird verwendet, um eine zufällige Gleitkommazahl zwischen zwei angegebenen Zahlen zu erzeugen. Zum Beispiel:

```Elixir
:random.uniform(1, 10)
# Output: 7.631079
```

Wenn Sie stattdessen eine ganzzahlige Zufallszahl benötigen, können Sie `:random.uniform/1` verwenden, um eine Ganzzahl zwischen null und dem angegebenen Wert zu erzeugen:

```Elixir
:random.uniform(100)
# Output: 42
```

Um eine zufällige Zahl zwischen 0 und 1 zu erzeugen, können Sie die Funktion `:random.uniform/0` verwenden:

```Elixir
:random.uniform()
# Output: 0.412242
```

Wenn Sie eine Liste von zufälligen Werten benötigen, können Sie die Funktion `Enum.random/1` verwenden, die eine Liste von Elementen nimmt und ein zufälliges davon zurückgibt:

```Elixir
list = ["a", "b", "c", "d", "e"]
Enum.random(list)
# Output: "c"
```

Sie können auch eine Liste von eindeutigen zufälligen Werten mit `Enum.shuffle/1` erstellen, die die Elemente der Liste zufällig neu anordnet:

```Elixir
list = [1, 2, 3, 4, 5]
Enum.shuffle(list)
# Output: [3, 4, 5, 1, 2]
```

## Deep Dive

Die Funktionen `:random.uniform/1` und `:random.uniform/2` nutzen den PRNG (Pseudorandom Number Generator) von Erlang, der auf dem Mersenne-Twister-Algorithmus basiert. Dieser PRNG hat eine sehr lange Periode und ist für die meisten Anwendungen ausreichend.

Für Anwendungen, bei denen eine hohe Sicherheit erforderlich ist, bietet Elixir auch die Funktion `:crypto.strong_rand_bytes/1`, die eine zufällige Bytefolge zurückgibt, die für die Kryptografie geeignet ist.

Ein wichtiger Aspekt der zufälligen Zahlenerzeugung ist die Initialisierung des PRNGs. Elixir verwendet dafür den Mersenne-Twister-Algorithmus in Kombination mit einem Seed, der standardmäßig auf den aktuellen Zeitstempel gesetzt wird. Es ist jedoch möglich, einen benutzerdefinierten Seed zu verwenden, indem man ihn als Argument für die Funktionen `:random.uniform/1` oder `:random.uniform/2` angibt.

Um einen benutzerdefinierten Seed zu verwenden, können Sie die Funktion `:random.seed/2` nutzen, die den Seed setzt und den aktualisierten PRNG zurückgibt:

```Elixir
seed = :random.seed(:random.uniform(), {1234, 5678, 9101112})
:random.uniform(seed)
# Output: 0.255910
```

Es ist wichtig zu beachten, dass die gleichen Seeds auch die gleichen Zahlenfolgen erzeugen werden. Wenn Sie also eine vorhersehbare Zahlenfolge vermeiden möchten, sollten Sie einen geeigneten Seed wählen.

## Siehe auch

Wenn Sie sich noch weiter in das Thema der zufälligen Zahlenerzeugung in Elixir vertiefen möchten, können Ihnen folgende Links helfen:

- [Elixir-Dokumentation zu Zufallszahlen](https://hexdocs.pm/elixir/random.html)
- [Elixir-Enum-Dokumentation](https://hexdocs.pm/elixir/Enum.html#random/1)
- [Elixir-Crypto-Dokumentation](https://hexdocs.pm/elixir/Crypto.html#strong_rand_bytes/1)