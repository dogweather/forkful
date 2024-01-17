---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Javascript: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Beim Programmieren bezieht sich das Interpolieren einer Zeichenfolge auf das Einsetzen von Variablen oder Ausdrücken in einen String. Es ist eine einfache und effektive Möglichkeit, Texte dynamisch zu gestalten und Daten in eine formatierte Zeichenfolge zu integrieren. Programmierer nutzen die Interpolation, um ihre Codes übersichtlicher und flexibler zu gestalten und um potenzielle Fehler zu vermeiden.

# Wie geht es:

Ein einfaches Beispiel für die String Interpolation wäre:

```Javascript
const name = "Lisa";
console.log(`Hallo ${name}, wie geht es dir?`);
```

Dieser Code würde die Ausgabe "Hallo Lisa, wie geht es dir?" erzeugen, da die Variable "name" in die Zeichenfolge eingefügt wird. Dabei wird der Variablenname in geschweiften Klammern innerhalb des Strings markiert und mit einem Dollarzeichen eingeleitet.

Ein weiteres Beispiel könnte die dynamische Erstellung von HTML-Elementen sein:

```Javascript
const title = "Interpolation";
const subtitle = "in Javascript";
const html = `<h1>${title}</h1><p>${subtitle}</p>`;
document.querySelector("#container").innerHTML = html;
```

Dieses Beispiel würde ein HTML-Dokument mit einem Titel und Untertitel erstellen, die jeweils aus den vorher definierten Variablen interpoliert werden.

# Tiefes Eintauchen:

Die String Interpolation ist ein relativ neues Konzept in der Javascript-Programmierung. Früher wurde sie durch das Verketten von Strings und Variablen mit dem Pluszeichen erreicht, was jedoch umständlicher und fehleranfälliger war. Alternativen zu String Interpolation in anderen Programmiersprachen sind beispielsweise der printf-Befehl in C oder das f-String-Format in Python.

Die Implementierung von String Interpolation erfolgt in der Regel durch die Verwendung von Backticks anstelle von Anführungszeichen und der Verwendung von geschweiften Klammern und Dollarzeichen innerhalb der Zeichenfolge. Dabei sollte beachtet werden, dass es sich bei dem Ausdruck innerhalb der geschweiften Klammern um eine JavaScript-Expression handeln muss.

# Weitere Informationen:

Weitere Informationen zur String Interpolation in Javascript können unter folgenden Links gefunden werden:

- [MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/template_strings)
- [W3Schools](https://www.w3schools.com/js/js_string_interpolation.asp)
- [Codecademy](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet)