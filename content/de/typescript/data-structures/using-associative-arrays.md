---
title:                "Verwendung von assoziativen Arrays"
aliases: - /de/typescript/using-associative-arrays.md
date:                  2024-01-30T19:13:14.746932-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays oder Objekte in TypeScript ermöglichen es Ihnen, mit Strings (oder Schlüsseln) auf Wertpaare zuzugreifen. Programmierer nutzen sie für dynamischere Datenzugriffsmuster im Vergleich zu traditionellen Arrays und bieten eine flexible Art, Daten zu strukturieren und darauf zuzugreifen, ohne an numerische Indizes gebunden zu sein.

## Wie man es macht:

Das Erstellen und Verwenden von assoziativen Arrays in TypeScript ist unkompliziert. Hier eine grundlegende Anleitung:

```TypeScript
// Ein assoziatives Array deklarieren
let user: { [key: string]: string } = {};

// Daten hinzufügen
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Ausgabe:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Das Iterieren über Schlüssel-Wert-Paare ist ebenfalls einfach:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Ausgabe:

```TypeScript
name: Jane Doe
email: jane@example.com
```

Und wenn Sie es mit einer Mischung aus Datentypen zu tun haben, ist das Typsystem von TypeScript nützlich:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Ausgabe:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Tiefergehend

In TypeScript sind das, was wir als assoziative Arrays bezeichnen, im Wesentlichen Objekte. Historisch gesehen sind in Sprachen wie PHP assoziative Arrays ein fundamentaler Typ, aber JavaScript (und damit auch TypeScript) verwendet für diesen Zweck Objekte. Dieser Ansatz ist sowohl eine Stärke als auch eine Einschränkung. Objekte bieten eine hochdynamische Struktur für die Zuordnung von Strings zu Werten, sind jedoch nicht dazu gedacht, im traditionellen Sinn als 'Arrays' verwendet zu werden. Beispielsweise können Sie Array-Methoden wie `push` oder `pop` nicht direkt auf diesen Objekten anwenden.

Für Fälle, in denen Sie geordnete Sammlungen von Schlüssel-Wert-Paaren mit array-ähnlichen Operationen benötigen, bietet TypeScript (und modernes JavaScript) das `Map`-Objekt:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Während das Typsystem von TypeScript und ES6-Features wie `Map` leistungsfähige Alternativen bieten, ist das Verständnis dafür, wie man Objekte als assoziative Arrays verwendet, nützlich für Szenarien, in denen Objektliterale effizienter sind oder wenn man mit JSON-Datenstrukturen arbeitet. Es geht darum, das richtige Werkzeug für den Job zu wählen.
