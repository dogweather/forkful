---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "Clojure: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Interpolation bezieht sich auf das Einsetzen von Variablen oder Expressions in einen String, um dynamisch generierte Inhalte zu erzeugen. Programmierer nutzen dies, um Texte mit variablen Werten zu erstellen, wie z.B. Fehlermeldungen, benutzerdefinierte Nachrichten oder dynamische URLs.

## So geht's:

Die Funktion `str` in Clojure kann verwendet werden, um Strings zu interpolieren. Sie nimmt beliebig viele Argumente und fügt sie in den String ein. Zum Beispiel:

```Clojure
(str "Hallo " "Welt!" " Ich bin " 25 "Jahre alt.")
```

Dies würde den folgenden String zurückgeben: `"Hallo Welt! Ich bin 25 Jahre alt."`

Ein weiteres praktisches Beispiel ist das Einbetten von Variablen in eine URL:

```Clojure
(def username "Max")

(str "https://www.example.com/users/" username)
```

Dies würde den folgenden String zurückgeben: `"https://www.example.com/users/Max"`

## Deep Dive:

Interpolation ist kein neues Konzept und wird bereits in anderen Programmiersprachen wie Python und Ruby verwendet. Eine Alternative zu `str` in Clojure ist die Funktion `format`, die es ermöglicht, Platzhalter in einem String zu definieren und diese mit entsprechenden Werten zu ersetzen. Zum Beispiel:

```Clojure
(def account-balance 1000)

(format "Ihr Kontostand beträgt %s Euro." account-balance)
```

Dies würde den folgenden String zurückgeben: `"Ihr Kontostand beträgt 1000 Euro."`

Die Implementierung von Interpolation in Clojure ist inspiriert von der LISP-Programmiersprache und nutzt die gleichen Konzepte wie beispielsweise `format` in Common Lisp oder `printf` in C.

## Siehe auch:

Weitere Informationen zu Strings und deren Verwendung in Clojure finden Sie in der [offiziellen Dokumentation](https://clojure.org/reference/strings).