---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:35.977817-07:00
description: "Assoziative Arrays, in anderen Sprachen auch als Maps oder W\xF6rterb\xFC\
  cher bekannt, sind Schl\xFCssel-Wert-Paare, die f\xFCr effiziente Datenabfrage und\u2026"
lastmod: '2024-03-11T00:14:28.253559-06:00'
model: gpt-4-0125-preview
summary: "Assoziative Arrays, in anderen Sprachen auch als Maps oder W\xF6rterb\xFC\
  cher bekannt, sind Schl\xFCssel-Wert-Paare, die f\xFCr effiziente Datenabfrage und\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, in anderen Sprachen auch als Maps oder Wörterbücher bekannt, sind Schlüssel-Wert-Paare, die für effiziente Datenabfrage und -manipulation genutzt werden. Anders als traditionelle Arrays, die Integer-Indizes verwenden, nutzen assoziative Arrays Schlüssel, was den Datenzugriff für Programmierer intuitiver und flexibler macht.

## Wie:

C bietet keine eingebaute Unterstützung für assoziative Arrays wie einige höherstufige Sprachen, aber man kann sie mit Hilfe von Strukturen und Hashing simulieren. Unten ist ein vereinfachtes Beispiel, das eine Kombination aus einer Struktur und einer einfachen Hashfunktion verwendet, um ein assoziatives Array für das Speichern und Zugreifen von Ganzzahlen über String-Schlüssel zu implementieren.

Definieren Sie zunächst eine Struktur, um ein einzelnes Schlüssel-Wert-Paar darzustellen, und eine weitere, um das assoziative Array selbst darzustellen:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Ausgabe: 1
    printf("%d\n", find(&a, "key2")); // Ausgabe: 2

    return 0;
}
```

Dieses Beispiel demonstriert grundlegende Operationen: Initialisierung eines assoziativen Arrays, Einfügen von Schlüssel-Wert-Paaren und das Finden von Werten über Schlüssel. Beachten Sie, dass dieser Code keine Kollisionserkennung beinhaltet und zu Bildungszwecken gedacht ist.

## Tiefergehend

Das Konzept der assoziativen Arrays geht C vor, aber die Low-Level-Natur der Sprache bietet keine direkte Unterstützung dafür als eingebaute Typen. Dies fördert ein tieferes Verständnis von Datenstrukturen und Algorithmen, einschließlich Hashing-Mechanismen für effiziente Schlüssel-Wert-Zuordnungen. Viele C-Bibliotheken und Frameworks bieten ausgefeiltere Ansätze für die Implementierung assoziativer Arrays, wie GLib’s `GHashTable`, das eine robuste Implementierung mit Kollisionserkennung, dynamischer Größenänderung und Unterstützung für beliebige Schlüssel- und Werttypen bietet.

Obwohl die manuelle Konstruktion von assoziativen Arrays in C im Vergleich zu Sprachen mit eingebauter Unterstützung als umständlich angesehen werden kann, bietet sie unschätzbare Einblicke in die Funktionsweise von Datenstrukturen und schärft die Fähigkeiten eines Programmierers im Problemlösen und Optimieren. Für Produktionscode oder komplexere Anwendungen ist jedoch die Nutzung vorhandener Bibliotheken wie GLib oft ein praktischerer und zeiteffizienterer Ansatz.
