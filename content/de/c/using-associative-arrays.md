---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-01-30T19:10:17.238508-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, oder Hash-Maps, sind Schlüssel-Wert-Paare, die es Ihnen ermöglichen, Daten mit einem Schlüssel zu speichern und abzurufen. Sie sind unglaublich nützlich in C, da sie einen schnelleren Datenzugriff im Vergleich zu Listen ermöglichen, besonders wenn Sie mit einer großen Menge von Daten arbeiten.

## Wie:

C hat keine eingebaute Unterstützung für assoziative Arrays wie einige andere Sprachen, aber wir können Strukturen und einige Bibliotheksfunktionen verwenden, um eine ähnliche Funktionalität zu erhalten. Hier ist eine einfache Implementierung mit der `uthash`-Bibliothek, die Sie in Ihr Projekt einbinden müssen.

Definieren Sie zuerst eine Struktur, um Ihre Schlüssel-Wert-Paare zu halten:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Das wird unser Schlüssel sein
    char name[10]; // Das ist der Wert, der mit unserem Schlüssel verknüpft ist
    UT_hash_handle hh; // Macht diese Struktur hashbar
} person;
```

Als Nächstes fügen wir einige Einträge hinzu und rufen sie ab:

```C
int main() {
    person *my_people = NULL, *s;

    // Einen Eintrag hinzufügen
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // Einen Eintrag abrufen
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    Wenn (s) {
        printf("Gefunden: %s\n", s->name);
    }
    
    return 0;
}
```

Beispielausgabe wäre:

```
Gefunden: Alice
```

Vergessen Sie nicht, den zugewiesenen Speicher freizugeben und die Hash-Tabelle am Ende zu deallocieren, um Speicherlecks zu vermeiden.

## Tiefere Einblicke

Obwohl assoziative Arrays nicht nativ in C vorhanden sind, füllen Bibliotheken wie `uthash` die Lücke recht gut und bieten eine ziemlich unkomplizierte Möglichkeit, diese Funktionalität zu nutzen. Historisch gesehen mussten C-Entwickler ihre Version dieser Datenstrukturen implementieren, was zu verschiedenen und oft komplexen Implementierungen führte, besonders für diejenigen, die gerade erst mit der Sprache anfingen.

Denken Sie daran, die Effizienz der Verwendung von assoziativen Arrays in C hängt stark davon ab, wie gut die Hash-Funktion Werte über die Tabelle verteilt, um Kollisionen zu minimieren. Während Bibliotheken wie `uthash` eine gute Balance zwischen Benutzerfreundlichkeit und Leistung bieten, möchten Sie in kritischen Anwendungen, in denen die Leistung von größter Bedeutung ist, vielleicht Ihre eigene Hash-Tabelle maßschneidern oder implementieren.

Für Anwendungen, die maximale Effizienz erfordern, könnten alternative Datenstrukturen oder sogar andere Programmiersprachen mit eingebauter Unterstützung für assoziative Arrays eine bessere Wahl sein. Jedoch, in vielen Situationen, besonders wenn Sie bereits in einer C-Umgebung arbeiten, bietet die Verwendung einer Bibliothek wie `uthash` eine praktische Balance zwischen Leistung und Bequemlichkeit.
