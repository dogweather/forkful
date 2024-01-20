---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Musterbasierte Zeichenlöschung in Javascript

## Was & Warum?

Musterbasiertes Löschen von Zeichen ist der Prozess, bei dem Zeichen, die einem bestimmten Muster entsprechen, in einem String entfernt werden. Programmierer tun dies, um Text zu reinigen oder zu formatieren.

## Wie geht das:

Hier ist ein einfaches Beispiel, wie Sie Zeichen matching a pattern entfernen können. Wir werden den eingebauten `replace`-Methode mit einem Regexp-Muster verwenden.

```Javascript
let text = "grund1egende5reste0";
let pattern = /\d/g; // Dieses Muster entspricht allen Zahlen

let cleanText = text.replace(pattern, "");

console.log(cleanText); // Ausgabe: "grundegendebenreste"
 ```

## Tiefgang:

Die Technik des muster-basierten Löschen von Zeichen gibt es schon seitdem die ersten String-Manipulationen in den frühen Programmiersprachen implementiert wurden. Die Implementation in Javascript ist sehr ähnlich wie in anderen höheren Sprachen wie Perl oder Python.

Alternativ zur `replace`-Methode kann man auch die `split`- und `join`-Methoden für das Löschen von Zeichen verwenden, was jedoch weniger effizient ist. Der Vorteil dieser Methode ist ihre Lesbarkeit.

```Javascript
let text = "grund1egende5reste0";
let cleanText = text.split('1').join('').split('5').join('').split('0').join('');

console.log(cleanText); // Ausgabe: "grundegendebenreste"
```

Die Implementierungsdetails des muster-basierten Zeichenlöschens in Javascript hängen von der genutzten Methode ab. Der `replace`-Befehl nutzt die effiziente Boyer-Moore String-Suchalgorithmen implementiert in den meisten modernen Javascript-Engines, während `split` und `join` einfach durch den String iterieren.

## Siehe auch:

Hier sind einige weitere Quellen für weiterführende Informationen:
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)

Strukturieren Sie Ihren Code gut und denken Sie logisch. Frohes Programmieren!