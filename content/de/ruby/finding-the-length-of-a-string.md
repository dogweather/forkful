---
title:    "Ruby: Die Länge eines Strings finden"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, um die Länge eines Strings in Ruby zu finden. Zum Beispiel kann es hilfreich sein, um die Eingabe von Benutzern zu überprüfen oder um bestimmte Manipulationen an einem String vorzunehmen. Egal aus welchem Grund, die Länge eines Strings ist ein wichtiges Konzept in der Ruby Programmierung.

## So geht's

Um die Länge eines Strings in Ruby zu finden, gibt es eine eingebaute Methode namens `length`. Diese Methode gibt einfach die Anzahl der Zeichen in einem String zurück. Schauen wir uns ein Beispiel an:

```Ruby
string = "Hallo, Welt!"
puts string.length
```
Output: `12`

Wie du sehen kannst, gibt die `length` Methode 12 zurück, da es 12 Zeichen in unserem String gibt. Diese Methode ist sehr einfach und macht das Finden der Länge eines Strings zum Kinderspiel.

## Tiefer tauchen

Es gibt noch einige weitere Dinge, die du über die Länge von Strings in Ruby wissen solltest. Zum Beispiel gibt es auch die `size` Methode, die das gleiche wie `length` tut. Die meisten verwenden jedoch `length`, da es als Standard betrachtet wird.

Außerdem gibt es auch die Möglichkeit, die Anzahl eines bestimmten Zeichens in einem String zu zählen. Dafür gibt es die `count` Methode, die als Argument das zu zählende Zeichen nimmt. Schauen wir uns ein Beispiel an:

```Ruby
string = "Hallo, Welt!"
puts string.count("l")
```
Output: `2`

In diesem Beispiel zählt die `count` Methode die Anzahl der Buchstabe "l" im String.

Manchmal kann es auch nützlich sein, die Länge eines Strings in Unicode Zeichen zu finden. Dafür gibt es die `codepoints` Methode, die eine Array mit den Codepoints des Strings zurückgibt. Wenn du die Anzahl dieser Elemente zählst, bekommst du die Länge des Strings in Unicode Zeichen.

## Siehe auch

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby String Methods](https://www.rubyguides.com/2018/10/ruby-string-methods/)

Ich hoffe, dieser kurze Einblick in die Länge von Strings in Ruby hat dir geholfen und du kannst dieses Konzept nun besser verstehen und nutzen. Schau dir auf jeden Fall auch die offizielle Dokumentation und andere Ressourcen an, um dein Wissen weiter zu vertiefen. Viel Spaß beim Coden!