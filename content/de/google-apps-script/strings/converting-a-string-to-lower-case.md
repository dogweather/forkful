---
title:                "Einen String in Kleinbuchstaben umwandeln"
aliases:
- /de/google-apps-script/converting-a-string-to-lower-case.md
date:                  2024-02-01T21:51:09.049923-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Umwandlung einer Zeichenkette in Kleinbuchstaben in Google Apps Script, einer cloudbasierten Skriptsprache zur Automatisierung von Aufgaben in Google-Produkten, ist eine grundlegende Aufgabe, die darauf abzielt, Textdaten zu standardisieren. Programmierer führen diese Aktion häufig durch, um Konsistenz bei Benutzereingaben, Datenverarbeitung oder beim Vergleichen von Strings zu gewährleisten, da sie Probleme mit der Groß-/Kleinschreibung eliminiert.

## Wie:

Die Umwandlung einer Zeichenkette in Kleinbuchstaben in Google Apps Script ist dank der integrierten JavaScript-Methoden, die in der Skriptumgebung verfügbar sind, unkompliziert. Die Methode `toLowerCase()` ist das, was Sie meist verwenden werden. So können Sie es implementieren:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // Gibt aus: hello, world!
}
```

Diese einfache Funktion demonstriert, wie ein Originalstring genommen, die Methode `toLowerCase()` angewendet und das Ergebnis protokolliert wird. Dies ist besonders nützlich, wenn mit Eingaben gearbeitet wird, die auf Groß-/Kleinschreibung nicht achten sollen. Zum Beispiel beim Vergleichen von E-Mail-Adressen, die Benutzer in verschiedenen Schreibweisen eingeben könnten.

Zusätzlich, für Situationen, in denen Sie mit Array-Daten arbeiten, können Sie jedes Element durchlaufen, um sie in Kleinbuchstaben umzuwandeln:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // Gibt aus: [alice, bob, charlie]
}
```

Dieses Beispiel betont die Vielseitigkeit von `toLowerCase()` beim Umgang mit Mehrfach-String-Daten und gewährleistet Einheitlichkeit in Ihrem Datensatz.

## Tiefere Einblicke

Die Methode `toLowerCase()`, die von JavaScript geerbt und innerhalb von Google Apps Script verwendet wird, ist seit den ersten Versionen von JavaScript ein integraler Bestandteil der Zeichenkettenmanipulation. Ihr Hauptzweck ist es, bei der Groß-/Kleinschreibung unempfindlichen Umgang mit textuellen Daten zu unterstützen, ein Bedürfnis, das mit dem Aufkommen von dynamischen, benutzerinteraktiven Webanwendungen entstand. Trotz ihrer Einfachheit spielt der Mechanismus eine entscheidende Rolle bei der Datenvalidierung, Sortierung und Suchalgorithmen, indem er die durch die Groß-/Kleinschreibung eingeführte Komplexität reduziert.

In Bezug auf die Leistung ist der Umwandlungsprozess in modernen JavaScript-Engines hoch optimiert; jedoch sollte seine Anwendung innerhalb groß angelegter Datenoperationen noch bedacht sein, um unnötigen Verarbeitungsaufwand zu vermeiden.

Eine Alternative, insbesondere bei der Arbeit mit komplexen Mustern oder wenn ortsbezogene Umwandlungen benötigt werden, ist die Methode `toLocaleLowerCase()`. Diese Variante berücksichtigt ortsbezogene Regeln für die Umwandlung von Zeichen in Kleinbuchstaben, was für Anwendungen, die mehrere Sprachen unterstützen, wesentlich sein könnte:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // Gibt aus: märz
```

Trotz der zusätzlichen Komplexität ist `toLocaleLowerCase()` ein leistungsfähiges Werkzeug für internationale Anwendungen und stellt sicher, dass die Umwandlung die sprachlichen Normen des Benutzerorts respektiert. Welche Methode Sie auch wählen, die Umwandlung von Zeichenketten in Kleinbuchstaben bleibt ein wesentlicher Bestandteil der Textverarbeitung in Google Apps Script und schließt die Lücke zwischen Benutzereingabe und standardisierter Datenbehandlung.
