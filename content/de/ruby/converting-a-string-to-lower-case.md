---
title:                "Ruby: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

In Ruby gibt es verschiedene Methoden, um Strings zu manipulieren. Eine dieser Methoden ist die Konvertierung in Kleinbuchstaben. In diesem Blog-Beitrag werden wir genauer betrachten, warum und wie man Strings in Kleinbuchstaben umwandelt und wir werden uns auch tiefer in die Funktionsweise dieser Methode eintauchen.

## Warum

Es gibt verschiedene Gründe, warum man eine String in Kleinbuchstaben umwandeln würde. Zum Beispiel kann es sein, dass man Benutzereingaben standardisieren möchte, um sicherzustellen, dass alle Eingaben im gleichen Format gespeichert werden. Oder man möchte einen String mit anderen Strings vergleichen, ohne auf die Groß- und Kleinschreibung achten zu müssen.

## Wie man Strings in Kleinbuchstaben umwandelt

Die einfachste Methode, um einen String in Kleinbuchstaben umzuwandeln, ist die Verwendung der `downcase`-Methode. Schauen wir uns das in einem Beispiel an:

```Ruby
string = "HALLO DORT"
puts string.downcase
```

Das oben genannte Beispiel würde den String "HALLO DORT" in "hallo dort" umwandeln und im Terminal ausgeben.

Man sollte jedoch beachten, dass die `downcase`-Methode nur die Großbuchstaben in Kleinbuchstaben umwandelt. Falls der String bereits ausschließlich aus Kleinbuchstaben besteht, bleibt er unverändert.

Es gibt auch andere Methoden wie `upcase` und `capitalize`, um einen String in andere Schreibweisen umzuwandeln. Es ist wichtig, die richtige Methode basierend auf den Anforderungen zu wählen.

## Tiefer Einblick

Die `downcase`-Methode verwendet die Unicode-Spezifikation, um Großbuchstaben in Kleinbuchstaben umzuwandeln. Dadurch ist sie auch in der Lage, Sonderzeichen und Buchstaben in anderen Sprachen umzuwandeln, nicht nur Englisch. Diese Methode ist auch nicht-destruktiv, d.h. sie ändert den ursprünglichen String nicht, sondern gibt lediglich einen neuen String mit den umgewandelten Buchstaben zurück.

Es gibt auch Möglichkeiten, einen String in Kleinbuchstaben umzuwandeln, ohne die `downcase`-Methode zu verwenden, z.B. durch die Verwendung der `map`-Methode auf einem Array von Buchstaben des Strings. Dies kann jedoch ineffizienter sein, besonders bei längeren Strings.

## Siehe auch

- [Ruby String Class](https://ruby-doc.org/core-3.0.0/String.html)
- [Unicode Character Database](http://www.unicode.org/ucd/)
- [Online-Demo zur Verwendung von String-Methoden](https://www.ruby-lang.org/en/documentation/quickstart/3/)

Vielen Dank fürs Lesen! Ich hoffe, dieser Blog-Beitrag hat dir geholfen, die `downcase`-Methode von Ruby besser zu verstehen und wie sie verwendet werden kann, um Strings zu manipulieren. Bis zum nächsten Mal!

## Siehe auch