---
date: 2024-01-26 03:38:45.842793-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, sich\
  \ von diesen l\xE4stigen doppelten oder einfachen Anf\xFChrungszeichen zu befreien,\
  \ die Ihren\u2026"
lastmod: '2024-02-25T18:49:50.607259-07:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String bedeutet, sich von\
  \ diesen l\xE4stigen doppelten oder einfachen Anf\xFChrungszeichen zu befreien,\
  \ die Ihren\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String bedeutet, sich von diesen lästigen doppelten oder einfachen Anführungszeichen zu befreien, die Ihren Text umschließen. Programmierer tun dies, um Daten zu säubern, Einheitlichkeit zu gewährleisten oder Strings für die Verarbeitung vorzubereiten, bei denen Anführungszeichen unerwünscht sind oder Fehler verursachen können.

## Wie zu:
In Clojure sind Strings unveränderlich, daher sprechen wir beim "Entfernen von Anführungszeichen" wirklich davon, einen neuen String ohne Anführungszeichen zu erstellen. Hier ist der Kniff mit `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Lassen wir die doppelten Anführungszeichen weg
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Und werfen wir die einfachen Anführungszeichen raus
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Beispielanwendung:
(remove-double-quotes "\"Hallo, Welt!\"") ; => "Hallo, Welt!"
(remove-single-quotes "'Hallo, Welt!'")   ; => "Hallo, Welt!"
```
Wollen Sie sowohl einzelne als auch doppelte Anführungszeichen auf einen Schlag behandeln? Schauen Sie sich das an:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Beispielanwendung:
(remove-quotes "\"Hallo, 'Clojure' Welt!\"") ; => "Hallo, Clojure Welt!"
```

## Tiefer Eintauchen
Zurück in der Zeit, als Daten unordentlicher waren als das Zimmer eines Kindes, waren Anführungszeichen in Strings die Norm, um Text zu kennzeichnen. Aber mit der Entwicklung der Informatik wurden Anführungszeichen mehr als nur Textbegrenzer – sie übernahmen syntaktische Rollen in Programmiersprachen.

Clojure, mit seinem Lisp-Erbe, verwendet Anführungszeichen nicht auf die gleiche Weise wie einige andere Sprachen. Sicher, sie dienen zur Kennzeichnung von Strings, aber sie haben auch eine spezielle Rolle bei der Erstellung von Literalen. Nichtsdestotrotz bleibt das Entfernen von Anführungszeichen aus Strings eine zeitlose Aufgabe.

Warum nicht einfach die Enden eines Strings abschneiden? Nun, das setzt voraus, dass Ihre Anführungszeichen immer den Anfang und das Ende Ihres Strings umarmen wie ein Paar übermäßig liebevolle Großeltern. Echte Daten sind unordentlicher. Hier kommen Regex (reguläre Ausdrücke) ins Spiel, die es Ihnen ermöglichen, diese Anführungszeichen zu zielen, egal wo sie sich verstecken.

Alternativen? Sicher, Sie können sich mit `subs`, `trim`, `triml`, `trimr` oder sogar Transducern schick machen, wenn Sie angeben möchten. Aber `replace` mit Regex ist wie ein Lichtschwert zu einem Messerkampf zu bringen – es kommt direkt zur Sache.

## Siehe auch
Wenn Ihr Gehirn nach mehr Clojure-Stringmanipulations-Güte juckt, könnten diese Brotkrumen helfen:

- ClojureDocs zu `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Reguläre Ausdrücke in Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java-Zwischensprache für die String-Verarbeitung (Clojure läuft immerhin auf der JVM): https://clojure.org/reference/java_interop#_working_with_strings

Hören Sie nicht auf, bei der Entfernung von Anführungszeichen stehen zu bleiben. Es gibt eine ganze Welt der String-Magie da draußen in Clojure-Land, die darauf wartet, entdeckt zu werden.
