---
title:                "Usando arrays asociativos"
aliases:
- /es/c/using-associative-arrays.md
date:                  2024-02-03T18:10:35.887326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays asociativos"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Los arreglos asociativos, conocidos en otros lenguajes como mapas o diccionarios, son pares clave-valor utilizados para la búsqueda y manipulación eficiente de datos. A diferencia de los arreglos tradicionales que usan índices enteros, los arreglos asociativos utilizan claves, haciendo el acceso a los datos más intuitivo y flexible para los programadores.

## Cómo hacerlo:

C no tiene soporte integrado para arreglos asociativos como algunos lenguajes de alto nivel, pero puedes simularlos utilizando estructuras y hashing. A continuación se muestra un ejemplo simplista utilizando una combinación de una estructura y una función de hashing simple para implementar un arreglo asociativo para almacenar y acceder a enteros por claves de cadena.

Primero, define una estructura para representar un solo par clave-valor y otra para representar el propio arreglo asociativo:

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

    printf("%d\n", find(&a, "key1")); // Salida: 1
    printf("%d\n", find(&a, "key2")); // Salida: 2

    return 0;
}
```

Este ejemplo demuestra operaciones básicas: inicializar un arreglo asociativo, insertar pares clave-valor y encontrar valores por claves. Nota que este código carece de manejo de colisiones y está destinado a fines educativos.

## Análisis Profundo

El concepto de arreglos asociativos precede a C, pero la naturaleza de bajo nivel del lenguaje no los soporta directamente como tipos integrados. Esto fomenta una comprensión más profunda de las estructuras de datos y algoritmos, incluyendo mecanismos de hashing para una mapeo clave-valor eficiente. Muchas bibliotecas y marcos de trabajo en C ofrecen enfoques más sofisticados para implementar arreglos asociativos, como `GHashTable` de GLib, que proporciona una implementación robusta completa con manejo de colisiones, redimensionamiento dinámico y soporte para tipos de clave y valor arbitrarios.

Aunque la construcción manual de arreglos asociativos en C puede verse como engorrosa en comparación con lenguajes con soporte integrado, ofrece una visión invaluable sobre el funcionamiento interno de las estructuras de datos, agudizando las habilidades de un programador en resolución de problemas y optimización. Sin embargo, para código de producción o aplicaciones más complejas, aprovechar bibliotecas existentes como GLib es a menudo un enfoque más práctico y eficiente en tiempo.
