---
title:                "Clojure: Zusammenführen von Zeichenfolgen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Konkatenation von Zeichenfolgen (englisch: string) ist eine wichtige Fähigkeit, die in vielen Programmiersprachen verwendet wird, einschließlich Clojure. Durch die Verkettung von Zeichenfolgen können wir Texte und Variablen miteinander kombinieren und so dynamische Ausgaben erzeugen. In diesem Beitrag werden wir uns ansehen, wie man Strings in Clojure verketten kann.

## Wie man Strings in Clojure verketten kann

Die Verkettung von Strings in Clojure ist sehr einfach und erfordert nur die Verwendung des Operators `str` sowie die Angabe der zu verkettenen Zeichenfolgen in Klammern. Hier ist ein Beispiel:

```Clojure
(str "Hallo" "Welt!") ; Ausgabe: "HalloWelt!"
```

Wie wir sehen können, werden die beiden übergebenen Zeichenfolgen ohne Leerzeichen verkettet. Wenn wir Leerzeichen oder andere Zeichen in die Ausgabe einfügen möchten, können wir dies mit dem `str` Operator tun, indem wir die Leerzeichen oder Zeichen in Anführungszeichen als eigene Zeichenfolge übergeben. Zum Beispiel:

```Clojure
(str "Hallo" " " "Welt!") ; Ausgabe: "Hallo Welt!"
```

Wir können auch Variablen in die Verkettung einbeziehen, indem wir sie als Argumente an den `str` Operator übergeben. Zum Beispiel:

```Clojure
(def name "Maria")
(str "Mein Name ist" name "!") ; Ausgabe: "Mein Name ist Maria!"
```

## Tief tauchen

Clojure bietet auch die Funktion `str-join`, mit der wir eine Liste von Zeichenfolgen durch ein spezifisches Trennzeichen verkettet können. Hier ist ein Beispiel:

```Clojure
(str-join "-" ["Mai" "Juni" "Juli"]) ; Ausgabe: "Mai-Juni-Juli"
```

Wir können auch eine bedingte Verkettung von Zeichenfolgen mit der Funktion `str-if` durchführen, bei der wir eine Bedingung angeben und angeben, was verknüpft werden soll, wenn die Bedingung erfüllt ist. Hier ist ein Beispiel:

```Clojure
(str-if true "Bonjour" "Hello") ; Ausgabe: "Bonjour"
(str-if false "Bonjour" "Hello") ; Ausgabe: "Hello"
```

## Siehe auch

- [Offizielle Clojure Dokumentation](https://clojure.org/guides/string_concatenation)
- [Clojure-Online-Tutorial](https://www.clojure.com/guides/learning-the-ropes/string-concatenation)