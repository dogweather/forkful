---
title:    "Fish Shell: String großschreiben"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Warum
Das Großschreiben von Zeichenfolgen kann eine nützliche Funktion in der Fish Shell sein, insbesondere wenn man mit Benutzereingaben oder Textverarbeitung arbeitet. In diesem Blogbeitrag zeigen wir Ihnen, wie Sie ganz einfach Zeichenfolgen in der Fish Shell großschreiben können.

## Wie geht das?
Die Grundidee ist, dass wir den `capitalize` Befehl verwenden, um die erste Zeichen in einem Wort großzuschreiben. Um jedoch eine ganze Zeichenfolge zu verarbeiten, müssen wir zunächst die Zeichenfolge in einzelne Worte aufteilen, dann das erste Zeichen jedes Wortes großschreiben und schließlich die Worte wieder zusammenführen. Wir können dies mit der `string split`, `map` und `string join` Funktion in der Fish Shell tun.

```Fish Shell
# Beispielzeichenfolge
set string "dies ist ein Beispiel"

# Zeichenfolge in Worte aufteilen
set words (string split -m 0 $string " ")

# Erstes Zeichen jedes Wortes großschreiben
set capitalized_words (map string capitalize $words)

# Worte wieder zusammenführen und Ausgabe drucken
echo (string join " " $capitalized_words)

# Ausgabe:
# Dies Ist Ein Beispiel
```

## Tiefer Einblick
Die `string split` Funktion ist nützlich, um eine Zeichenfolge in mehrere Teile zu zerlegen, basierend auf einem Trennzeichen. In unserem Beispiel verwenden wir das Leerzeichen als Trennzeichen, aber es können auch andere Zeichen verwendet werden.

Die `map` Funktion ist eine leistungsstarke Funktion, die auf jedes Element einer Liste angewendet werden kann. In unserem Fall wenden wir die `string capitalize` Funktion auf jedes Wort in unserer Liste an.

Die `string join` Funktion ist das Gegenteil von `string split` und ermöglicht es uns, eine Liste von Wörtern wieder zu einer einzigen Zeichenfolge zusammenzuführen, wobei ein angegebenes Trennzeichen zwischen jedem Wort hinzugefügt wird.

In diesem Beispiel haben wir eine einfache Methode gezeigt, um Zeichenfolgen in der Fish Shell großzuschreiben, aber es gibt auch andere Möglichkeiten, dies zu tun, je nachdem, welche spezifischen Anforderungen Sie haben. Weitere Informationen zu den verschiedenen Funktionen und Optionen finden Sie in der [Fish Shell Dokumentation](https://fishshell.com/docs/current/).

# Siehe auch
- Fish Shell Dokumentation: https://fishshell.com/docs/current/
- Fish Shell Referenzkarte: https://fishshell.com/docs/current/cmds.html