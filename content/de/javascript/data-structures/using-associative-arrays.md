---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-01-30T19:11:23.817917-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, oder wie sie in JavaScript genauer bekannt sind, Objekte, ermöglichen es Ihnen, Schlüssel mit Werten zu verbinden. Das ist besonders nützlich, wenn Sie eine Sammlung von Elementen benötigen, auf die Sie über spezifische Namen (Schlüssel) statt numerischer Indizes zugreifen möchten, was Ihren Code lesbarer und flexibler macht.

## Wie geht das:

Das Erstellen und Verwenden von assoziativen Arrays (Objekten) in JavaScript ist unkompliziert. Sie definieren ein Objekt mit geschweiften Klammern `{}`, und innerhalb dieser können Sie eine Reihe von Schlüssel-Wert-Paaren definieren. Schlüssel sind immer Zeichenketten, und Werte können alles sein: Zeichenketten, Zahlen, Arrays, sogar andere Objekte.

```javascript
// Ein assoziatives Array erstellen
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Elemente zugreifen
console.log(userInfo.name); // Ausgabe: Alex
console.log(userInfo["email"]); // Ausgabe: alex@example.com

// Neue Elemente hinzufügen
userInfo.job = "Entwickler";
userInfo["land"] = "Kanada";

console.log(userInfo);
/* Ausgabe:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Entwickler",
  land: "Kanada"
}
*/

// Ein Element löschen
delete userInfo.age;
console.log(userInfo);
/* Ausgabe:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Entwickler",
  land: "Kanada"
}
*/
```

Wie Sie sehen können, ist der Zugriff, das Hinzufügen oder Löschen von Elementen in einem assoziativen Array ziemlich direkt und intuitiv.

## Tiefergehend

In der JavaScript-Welt, obwohl wir oft den Begriff "assoziatives Array" hören, ist es technisch gesehen ein falscher Name, denn JavaScript verfügt nicht über echte assoziative Arrays wie andere Sprachen (z.B. PHP). Was JavaScript hat, sind Objekte, die einem ähnlichen Zweck dienen, aber eine leistungsfähigere und flexiblere Konstruktion sind.

Historisch gesehen wurden Arrays in Programmiersprachen entwickelt, um eine Sammlung von Elementen zu halten, auf die durch ihren numerischen Index zugegriffen wurde. Doch mit der Entwicklung der Softwareentwicklung entstand die Notwendigkeit für flexiblere Datenstrukturen. Assoziative Arrays oder Wörterbücher in anderen Sprachen waren eine Antwort und ermöglichten den Zugriff auf Elemente durch beliebige Schlüssel.

JavaScripts Ansatz mit Objekten als Schlüssel-Wert-Speichern bietet eine Mischung aus Funktionalität. Es erlaubt das Hinzufügen, Entfernen und Nachschlagen von Eigenschaften (Schlüssel) nach Namen. JSON (JavaScript Object Notation) ist ein Zeugnis für den Nutzen dieser Struktur und wurde zum de facto Standard für den Datenaustausch im Web.

Obwohl Objekte die meisten Bedürfnisse für assoziative Arrays abdecken, bietet das `Map`-Objekt, das in ES6 eingeführt wurde, in Fällen, in denen die Schlüsselreihenfolge oder Iteration wichtig ist, eine bessere Alternative. Ein `Map` behält die Schlüsselreihenfolge bei, akzeptiert eine breitere Palette von Datentypen als Schlüssel und beinhaltet hilfreiche Methoden für Iteration und Größenabfrage. Trotz dieser Vorteile bleibt die traditionelle Objektsyntax für ihre Einfachheit und Benutzerfreundlichkeit in vielen gängigen Szenarien beliebt.
