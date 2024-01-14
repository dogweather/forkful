---
title:                "TypeScript: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Warum
Einige von euch haben vielleicht schon einmal eine Website heruntergeladen, sei es für Archivierungszwecke oder um offline darauf zuzugreifen. Aber wusstet ihr, dass ihr auch in TypeScript eine Website herunterladen könnt? In diesem Artikel zeigen wir euch, warum ihr das tun würdet und wie ihr es in TypeScript machen könnt.

# Wie es geht
Bevor wir in die Details eintauchen, müssen wir zunächst das Modul "http" aus Node.js importieren. Dadurch haben wir Zugriff auf die "get" Funktion, die benötigt wird, um eine Website herunterzuladen.

```TypeScript
import * as http from 'http';

// URL der Website, die heruntergeladen werden soll
const url = 'https://www.beispielwebsite.com';

// Die "get" Funktion aus dem "http" Modul verwenden
http.get(url, (resp) => {
  let data = '';

  // Daten in Stücke aufteilen, wenn sie empfangen werden
  resp.on('data', (chunk) => {
    data += chunk;
  });

  // Komplette Antwort empfangen
  resp.on('end', () => {
    // Antwort in eine Datei schreiben
    // In diesem Fall wird die Datei "heruntergeladene_website.html" genannt
    fs.writeFile('heruntergeladene_website.html', data, (err) => {
      if (err) throw err;
      console.log('Website erfolgreich heruntergeladen!');
    });
  });
}).on("error", (err) => {
  console.log("Fehler beim Herunterladen der Website: " + err.message);
});
```

Der obige Code zeigt einen einfachen Ansatz, um eine Website herunterzuladen. Wir importieren das "http" Modul, erstellen eine Variable für die URL und verwenden die "get" Funktion, um die Website herunterzuladen. Dann speichern wir die Daten in einer Variablen, die "chunk" für "Stück" steht, da die Daten in Stücke aufgeteilt werden, wenn sie empfangen werden. Wir fügen jedes dieser Stücke zur Variablen "data" hinzu und wenn die komplette Antwort empfangen wurde, schreiben wir sie in eine Datei.

# Tiefere Einblicke
Um die heruntergeladene Website korrekt anzeigen zu können, müssen wir einige zusätzliche Schritte ausführen. Bevor wir die Antwort in eine Datei schreiben, müssen wir sicherstellen, dass sie im richtigen Format ist. Dafür müssen wir die Zeichenkodierung (z.B. UTF-8) sowie den Content-Type (z.B. text/html) der empfangenen Daten überprüfen und sie gegebenenfalls korrigieren. Dies kann erreicht werden, indem wir die "setEncoding" Funktion und die "headers" Variable verwenden.

```TypeScript
// Code vor dem Aufruf der "writeFile" Funktion
// sicherstellen, dass Zeichenkodierung und Content-Type korrekt sind
resp.setEncoding('utf8');

const headers = resp.headers;

// Falls Server die Zeichenkodierung nicht bereitstellt,
// verwenden wir den Standardwert UTF-8
const charset = (headers['content-type'].match(/charset=(.+)/) || [])[1] || 'utf-8';

// Inhalte mit der richtigen Zeichenkodierung umwandeln
data = iconv.decode(data, charset);
```

Mit diesen zusätzlichen Schritten können wir sicherstellen, dass die heruntergeladene Website korrekt angezeigt wird, unabhängig von der Zeichenkodierung und dem Content-Type, der vom Server bereitgestellt wird.

# Siehe auch
- [Einführung in TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Node.js-Dokumentation zum "http" Modul](https://nodejs.org/api/http.html)
- [Erlernen der Grundlagen von Node.js-Modulen](https://www.digitalocean.com/community/tutorials/how-to-create-your-first-node-js-module)