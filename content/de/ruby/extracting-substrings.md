---
title:                "Ruby: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum
Substring-Extraktion ist ein nützliches Konzept für Ruby-Programmierer, da es ermöglicht, eine Teilzeichenfolge aus einer größeren Zeichenfolge herauszufiltern. Dies kann hilfreich sein, um bestimmte Informationen aus einem längeren Text zu extrahieren oder um einen String in kleinere Abschnitte aufzuteilen.

## Wie man es macht
Um eine Substring-Extraktion in Ruby durchzuführen, verwenden wir die `slice` Methode. Hier ist ein Beispiel, in dem wir die Wörter "Hallo Welt" aus einem längeren Satz extrahieren:

```Ruby 
langer_satz = "Heute ist ein wunderschöner Tag und ich sage: Hallo Welt"
puts langer_satz.slice(37..46)
```

Die Ausgabe dieses Codes wäre "Hallo Welt", da wir den String von der 37. bis zur 46. Stelle schneiden.

## Tiefere Einblicke
Im obigen Beispiel haben wir den Substring explizit angegeben, indem wir die spezifischen Stellen angegeben haben, zwischen denen der String geschnitten werden soll. Aber es ist auch möglich, die Substring-Extraktion dynamischer zu gestalten, indem wir die `index` Methode verwenden, um die Stellen zu finden, an denen der Substring beginnen und enden soll. Hier ist ein Beispiel:

```Ruby
langer_satz = "Ich liebe Ruby-Programmierung"
start = langer_satz.index("Ruby") # findet die Stelle, an der "Ruby" beginnt
ende = langer_satz.index("rung") + 3 # findet die Stelle, an der "rung" endet
puts langer_satz.slice(start..ende) # gibt den Substring "Ruby-Programmierung" aus
```

Wir können auch die `split` Methode verwenden, um einen String an bestimmten Trennzeichen aufzuteilen und dann den entsprechenden Index zu verwenden, um den gewünschten Substring auszuwählen. Hier ist ein Beispiel:

```Ruby
langer_satz = "Ruby ist die beste Programmiersprache der Welt"
worte = langer_satz.split(" ") # teilt den String an jedem Leerzeichen auf
puts worte[3] # gibt den 3. Substring, also "beste", aus
```

# Siehe auch 
- [Ruby-Dokumentation zu `slice`](https://ruby-doc.org/core-2.6.1/String.html#method-i-slice)
- [Weitere Tipps zur Verwendung von Substrings in Ruby](https://www.rubyguides.com/2018/10/ruby-substring/)