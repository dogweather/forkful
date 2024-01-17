---
title:                "Zeichenketten verknüpfen"
html_title:           "Ruby: Zeichenketten verknüpfen"
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Hast du schon einmal von der Konkatenation von Zeichenketten gehört und dich gefragt, was das bedeutet und warum Programmierer das machen? Nun, Konkatenation ist einfach das Aneinanderhängen von Zeichenketten und es ist eine nützliche Technik, um das Erstellen von Textausgaben in deinem Code zu vereinfachen.

## Wie geht das?
In Ruby gibt es verschiedene Wege, um Strings zu konkatenieren. Einer davon ist der "+" Operator, der zwei Strings zusammenfügt. Du kannst auch die Methode "concat" verwenden, die die gleiche Funktionalität bietet. Hier ist ein Beispiel, wie du Strings konkatenieren kannst:

```Ruby
first_name = "Max"
last_name = "Mustermann"
full_name = first_name + last_name
puts full_name
```
Die Ausgabe wird sein: "MaxMustermann".

Du kannst auch Strings mit anderen Datenarten konkatenieren, es sei denn, sie sind Zahlen. In diesem Fall müssen Sie die Methode "to_s" verwenden, um die Zahl in einen String umzuwandeln. Hier ist ein Beispiel:

```Ruby
age = 25
puts "Ich bin " + age.to_s + " Jahre alt."
```
Die Ausgabe wird sein: "Ich bin 25 Jahre alt."

## Tiefere Einblicke
Die Konkatenation von Strings ist in der Programmierung seit langem eine gängige Methode. Vor der Verwendung von "concat" wurde oft ein "&" Symbol verwendet, um Strings zu verbinden. Diese Methode ist jedoch veraltet und wird nicht mehr so häufig verwendet.

Es gibt auch alternative Methoden, um Strings in Ruby zu konkatenieren, wie zum Beispiel der "<<" Operator, der auch zur Verbindung von Arrays verwendet werden kann. Es ist jedoch wichtig zu beachten, dass das Konkatenieren von Strings in großen Mengen ineffizient sein kann und es daher besser ist, andere Methoden wie String-Interpolation zu verwenden, um Textausgaben zu erstellen.

In der Implementierung verwendet Ruby die Methode "concat" intern, wenn der "+" Operator verwendet wird. Es ist auch wichtig zu beachten, dass Strings in Ruby unveränderlich sind, was bedeutet, dass die Konkatenation von Strings immer neue Objekte erstellen wird und die ursprünglichen Strings unverändert bleiben.

## Sieh dir auch an
* [String-Interpolation in Ruby](https://www.rubyguides.com/ruby-tutorial/string-interpolation/)
* [Die offizielle Ruby-Dokumentation für Strings](https://ruby-doc.org/core-3.0.0/String.html)
* [Alternative Methoden zur Manipulation von Strings in Ruby](https://www.rubyguides.com/2019/05/ruby-string-methods/)