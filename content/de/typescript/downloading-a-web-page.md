---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Artikel: Herunterladen einer Webseite mit TypeScript

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess des Abrufens von Daten von einem Server zur Anzeige in einem Webbrowser. Programmierer tun dies, um Inhalte von Webseiten zu analysieren, zu manipulieren oder zu speichern.

## Wie geht das?

Es gibt mehrere Wege eine Webseite mit TypeScript herunterzuladen. Hier ist eine einfache Methode mit Node.js und der Axios-Bibliothek.

1. Installiere die benötigten Bibliotheken:
```TypeScript
npm install axios typescript ts-node
```
2. Erstelle eine neue TypeScript-Datei `download.ts` und füge folgenden Code ein:

```TypeScript
import axios from 'axios';

const downloadPage = async (url: string): Promise<void> => {
    try {
        const response = await axios.get(url);
        console.log(response.data);
    } catch (error) {
        console.error(`Fehler beim Herunterladen der Seite: ${error}`);
    }
};

downloadPage('https://beispiel.de');
```
Dieser Code wird die angegebene Webseite herunterladen und ihren Inhalt auf der Konsole ausgeben.

## Deep Dive

Historisch gesehen, bevor Libraries wie Axios oder Fetch existierten, haben Entwickler das `http`-Modul von Node.js verwendet, um HTTP-Anfragen zu machen und Webseiten herunterzuladen. Dies kann immer noch eine gute Alternative sein, wenn du auf Drittanbieterbibliotheken verzichten möchtest.

Die Implementierungsdetails variieren, abhängig von der verwendeten Bibliothek und der Struktur der Webseite. Grundsätzlich sendet das Programm eine HTTP-Get-Anfrage an den Server, erwartet eine Antwort und verarbeitet dann die erhaltenen Daten.

## Siehe Auch

Hier sind einige Ressourcen, die dir bei der Arbeit helfen könnten:

1. [Axios-Dokumentation](https://github.com/axios/axios)
2. [Node.js http Module](https://nodejs.de/api/http.html)
3. [Fetch API](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)