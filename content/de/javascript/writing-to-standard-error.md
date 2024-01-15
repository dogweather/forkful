---
title:                "Schreiben auf standardmäßigen Fehler"
html_title:           "Javascript: Schreiben auf standardmäßigen Fehler"
simple_title:         "Schreiben auf standardmäßigen Fehler"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Schreiben von Standardfehlern in Javascript beschäftigen? Nun, es gibt verschiedene Gründe, warum man dies tun könnte. 

Eine mögliche Situation wäre, dass du als Entwickler arbeitest und du möchtest sicherstellen, dass dein Code ohne Probleme läuft, bevor du ihn auf die Produktionsumgebung hochlädst. In diesem Fall kannst du den Fehler in der "console" deines Browsers ausgeben lassen, um zu sehen, wo genau der Fehler auftritt. Eine andere Möglichkeit ist, dass du deine Anwendung fehlerfrei halten möchtest, indem du der "console" Ausgaben hinzufügst, um zu sehen, ob alles wie geplant funktioniert. 

Wie auch immer deine Gründe aussehen mögen, lassen uns nun einen Blick darauf werfen, wie man in Javascript Fehler ausgibt.

## Wie man Standardfehler ausgibt

Um einen Standardfehler in Javascript auszugeben, musst du die "console.error()" Funktion verwenden. Hier ist ein Beispiel, wie du dies in deinem Code einfügen kannst:

```Javascript
let num1 = 5;
let num2 = 0;

if (num2 === 0) {
  console.error("Divison durch 0 ist nicht möglich!");
} else {
  console.log(num1 / num2);
}
```

Das obige Beispiel zeigt, wie du einen Fehler ausgibst, wenn du versuchst, durch 0 zu teilen. Wenn du dieses Beispiel in deinem Code ausführst, wird die Fehlermeldung "Division durch 0 ist nicht möglich!" in der Konsole ausgegeben und dein Code wird nicht abstürzen. 

Es ist auch möglich, Variablen in der Fehlermeldung anzuzeigen, indem du sie als weitere Parameter an die "console.error()" Funktion übergibst:

```Javascript
let name = "Max";
let age = 25;

console.error("Der Benutzer " + name + " ist " + age + " Jahre alt.");
```

Dies würde die folgende Fehlermeldung ausgeben: "Der Benutzer Max ist 25 Jahre alt." 

Man kann auch komplexe Objekte als Parameter an "console.error()" übergeben, um mehr Informationen über den Fehler zu erhalten. Dies ist besonders nützlich bei der Fehlersuche in komplexen Anwendungen.

## Tiefergehende Informationen über das Schreiben von Standardfehlern

Wenn du ein tieferes Verständnis dafür entwickeln möchtest, wie das Schreiben von Standardfehlern in Javascript funktioniert, gibt es einige wichtige Dinge zu beachten.

Zunächst einmal wird "console.error()" auch eine "Stacktrace" in die Konsole ausgeben, was ein nützliches Werkzeug ist, um die Ursache eines Fehlers zu finden. Der "Stacktrace" zeigt die Reihenfolge der Funktionsaufrufe an, die zu dem Fehler geführt haben, und kann dir helfen, den genauen Punkt in deinem Code zu finden, an dem der Fehler aufgetreten ist. 

Ebenfalls wichtig ist, dass "console.error()" nur in Entwicklertools ausgegeben wird, nicht in der tatsächlichen Ausführungsumgebung deiner Anwendung. Wenn du also auf eine Live-Website gehst und in die Browserkonsole schaust, wirst du dort keine Fehlermeldungen sehen. Aus diesem Grund ist es empfehlenswert, "console.error()" Ausgaben nur in der Entwicklungsphase zu verwenden und sie später aus dem Code zu entfernen, bevor du deine Anwendung live schaltest.

## Siehe auch

- [Javascript Fehlerbehandlung: Ein Anfängerleitfaden](https://www.digitalocean.com/community/tutorials/js-exception-handling)
- [Konsole Ausgabe in Javascript](https://www.w3schools.com/js/js_console.asp)
- [Javascript Debugging Tipps](https://blog.logrocket.com/10-tips-for-javascript-debugging-like-a-pro/)