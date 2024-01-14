---
title:    "Ruby: Strings verknüpfen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

In der Programmierung müssen wir oft verschiedene Strings miteinander verbinden, um eine bestimmte Ausgabe zu erhalten. Dabei ist die Verkettung von Strings eine grundlegende Fähigkeit, die jeder Entwickler beherrschen sollte. In diesem Blog-Beitrag werden wir uns ansehen, warum es wichtig ist, Strings zu verbinden und wie man es in Ruby macht.

## Wie es geht

In Ruby gibt es verschiedene Möglichkeiten, Strings zu verbinden. Die einfachste Methode ist die Verwendung des `+` Operators. Sehen wir uns ein Beispiel an:

```Ruby
first_name = "Anna"
last_name = "Müller"
puts first_name + " " + last_name
```
Die Ausgabe wird `Anna Müller` sein, da die drei Strings miteinander verbunden und mit Leerzeichen getrennt werden.

Eine weitere Möglichkeit ist die Verwendung der `<<` Methode. Diese fügt den zweiten String an den ersten an. Sehen wir uns auch hierzu ein Beispiel an:

```Ruby
first_name = "Max"
last_name = "Schmidt"
first_name << " " << last_name
```
Die Ausgabe wird ebenfalls `Max Schmidt` sein. Dabei wird jedoch der erste String verändert, da der zweite String direkt daran angehängt wird.

Eine dritte Methode ist die Verwendung von String Interpolation. Hierbei setzen wir den zu verbindenden String in `#{}` innerhalb eines anderen Strings ein. Beispiel:

```Ruby
first_name = "Sara"
last_name = "Wagner"
puts "Mein Name ist #{first_name} #{last_name}"
```
Die Ausgabe wird `Mein Name ist Sara Wagner` sein.

## Deep Dive

Wenn wir uns die Implementierung von Strings in Ruby genauer anschauen, werden wir feststellen, dass sie eigentlich ein Objekt mit vielen Methoden sind. Die `+` und `<<` Methoden, die wir oben verwendet haben, sind nur zwei davon. Auch die String Interpolation ist eine Methode, die unter der Haube verwendet wird.

Eine nützliche Methode für die Verkettung von Strings ist `concat`. Diese fügt den gegebenen String an den ursprünglichen String an und gibt anschließend den ursprünglichen String zurück. Beispiel:

```Ruby
first_name = "Lisa"
last_name = "Schneider"
puts first_name.concat(" ").concat(last_name)
```
Die Ausgabe wird `Lisa Schneider` sein.

Es ist auch möglich, eine beliebige Anzahl von Strings miteinander zu verbinden, indem wir die `+` Methode mehrmals hintereinander aufrufen. Beispiel:

```Ruby
first_name = "Tom"
last_name = "Müller"
puts first_name + ", " + last_name + " " + "hat gerade das Spiel gewonnen!"
```
Die Ausgabe wird `Tom, Müller hat gerade das Spiel gewonnen!` sein.

Einige weitere Methoden, die bei der Verkettung von Strings hilfreich sein können, sind `insert`, `prepend` und `replace`. Es gibt also viele Möglichkeiten und Techniken, um Strings zu verbinden.

## Siehe auch

* [Ruby String Klasse Dokumentation](https://ruby-doc.org/core/String.html)
* [Das offizielle Ruby Tutorial zu Strings](https://ruby-doc.org/docs/Tutorial/part_02/control_structures.html#strings)