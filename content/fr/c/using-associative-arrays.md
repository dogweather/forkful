---
title:                "Utilisation des tableaux associatifs"
aliases:
- fr/c/using-associative-arrays.md
date:                  2024-02-03T18:10:43.132448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Les tableaux associatifs, connus dans d'autres langues sous le nom de cartes ou dictionnaires, sont des paires clé-valeur utilisées pour la recherche et la manipulation efficaces des données. Contrairement aux tableaux traditionnels qui utilisent des index entiers, les tableaux associatifs utilisent des clés, rendant l'accès aux données plus intuitif et flexible pour les programmeurs.

## Comment faire :

Le langage C ne dispose pas d'un support intégré pour les tableaux associatifs comme certains langages de plus haut niveau, mais vous pouvez les simuler à l'aide de structures et de hachage. Ci-dessous, un exemple simpliste utilisant une combinaison d'une structure et d'une fonction de hachage simple pour implémenter un tableau associatif permettant de stocker et d'accéder à des entiers par des clés de chaîne de caractères.

D'abord, définissez une structure pour représenter une unique paire clé-valeur et une autre pour représenter le tableau associatif lui-même :

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

    printf("%d\n", find(&a, "key1")); // Sortie : 1
    printf("%d\n", find(&a, "key2")); // Sortie : 2

    return 0;
}
```

Cet exemple démontre les opérations de base : l'initialisation d'un tableau associatif, l'insertion de paires clé-valeur, et la recherche de valeurs par les clés. Notez que ce code manque de gestion de collisions et est destiné à des fins éducatives.

## Examen approfondi

Le concept des tableaux associatifs préexiste au C, mais la nature de bas niveau du langage ne les supporte pas directement comme types intégrés. Cela encourage une compréhension plus profonde des structures de données et des algorithmes, y compris les mécanismes de hachage pour une cartographie clé-valeur efficace. De nombreuses bibliothèques et cadres de travail en C offrent des approches plus sophistiquées pour implémenter des tableaux associatifs, comme `GHashTable` de GLib, qui fournit une implémentation robuste complète avec gestion des collisions, redimensionnement dynamique, et prise en charge de types de clés et de valeurs arbitraires.

Alors que la construction manuelle de tableaux associatifs en C peut être vue comme fastidieuse par rapport aux langues avec un support intégré, elle offre des aperçus inestimables sur le fonctionnement interne des structures de données, aiguisant les compétences d’un programmeur en résolution de problèmes et en optimisation. Cependant, pour le code de production ou des applications plus complexes, l'utilisation de bibliothèques existantes comme GLib est souvent une approche plus pratique et efficace en termes de temps.
