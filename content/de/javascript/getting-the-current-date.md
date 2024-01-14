---
title:                "Javascript: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Die aktuelle Datum und Uhrzeit sind in der Webentwicklung eine häufig verwendete Funktion. Sie wird verwendet, um dynamische Inhalte wie Benutzeranmeldungen, Chatunterhaltungen und Nachrichten zu stempeln. Das Abrufen des aktuellen Datums ist auch wichtig, um geplante Aufgaben wie Benachrichtigungen oder Updates durchzuführen. In diesem Blogbeitrag werden wir uns anschauen, wie man mit Javascript das aktuelle Datum abrufen kann.

# Wie es geht

### Verwendung von Date()

Javascript bietet die Date() -Funktion, um das aktuelle Datum und die Uhrzeit zu erhalten. Dies funktioniert, indem ein neues Date-Objekt erstellt wird und dann die Methoden getDate(), getMonth(), getFullYear() usw. verwendet werden, um die Nummern für Datum, Monat und Jahr zu erhalten. Hier ist ein Beispiel:

```Javascript
let datum = new Date();
let tag = datum.getDate();
let monat = datum.getMonth() + 1; // Monate beginnen bei 0, daher muss 1 addiert werden
let jahr = datum.getFullYear();

console.log(`Heute ist der ${tag}.${monat}.${jahr}.`);
// Ausgabe: Heute ist der 25.12.2020.
```

### Verwendung von toLocaleDateString()

Eine andere Möglichkeit, das aktuelle Datum in einem bestimmten Format zu erhalten, ist die Verwendung der toLocaleDateString() -Funktion. Diese Funktion konvertiert das Datum in ein lesbares Format basierend auf den Browsereinstellungen des Benutzers. Hier ist ein Beispiel:

```Javascript
let datum = new Date();
let heute = datum.toLocaleDateString();

console.log(`Heute ist der ${heute}.`);
// Ausgabe: Heute ist der 25.12.2020.
```

### Verwendung von Moment.js

Moment.js ist eine beliebte Bibliothek für das Arbeiten mit Datum und Uhrzeit in Javascript. Um es zu verwenden, muss es zuerst eingebunden werden. Anschließend können Sie die Funktionen von Moment.js verwenden, um das aktuelle Datum und die Uhrzeit zu erhalten und sie in verschiedenen Formaten anzuzeigen. Hier ist ein Beispiel:

```Javascript
let moment = require('moment'); // Moment.js einbinden
let jetzt = moment();

console.log(`Es ist ${jetzt.format("MMMM Do YYYY, h:mm:ss a")}.`);
// Ausgabe: Es ist December 25th 2020, 12:00:00 am.
```

# Tief eintauchen

Es gibt noch viele weitere Funktionen und Methoden, mit denen Sie das aktuelle Datum und die Uhrzeit in Javascript abrufen können. Hier sind einige zusätzliche Ressourcen, die Sie erkunden können:

- [MDN Dokumentation über die Date()-Funktion](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Dokumentation über die toLocaleDateString()-Funktion](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Moment.js Dokumentation](https://momentjs.com/)

# Siehe auch

- [Wie man mit Javascript Arrays arbeitet](https://www.example.com/artikel/javascript-arrays)
- [Die Grundlagen der Datenvalidierung in Javascript](https://www.example.com/artikel/datenvalidierung-javascript)