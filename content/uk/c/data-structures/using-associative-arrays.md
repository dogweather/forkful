---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:32.870682-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : C \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\
  \u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0430\u0441\
  \u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\u0438\
  \u0432\u0456\u0432, \u043D\u0430 \u0432\u0456\u0434\u043C\u0456\u043D\u0443 \u0432\
  \u0456\u0434 \u0434\u0435\u044F\u043A\u0438\u0445 \u043C\u043E\u0432 \u0432\u0438\
  \u0449\u043E\u0433\u043E \u0440\u0456\u0432\u043D\u044F, \u0430\u043B\u0435 \u0432\
  \u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0441\u0438\u043C\u0443\u043B\u044E\
  \u0432\u0430\u0442\u0438 \u0457\u0445 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u0430\u043D\u043D\u044F \u0437\u0430\u2026"
lastmod: '2024-03-13T22:44:50.131548-06:00'
model: gpt-4-0125-preview
summary: "C \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432, \u043D\u0430 \u0432\u0456\u0434\u043C\u0456\u043D\u0443\
  \ \u0432\u0456\u0434 \u0434\u0435\u044F\u043A\u0438\u0445 \u043C\u043E\u0432 \u0432\
  \u0438\u0449\u043E\u0433\u043E \u0440\u0456\u0432\u043D\u044F, \u0430\u043B\u0435\
  \ \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0441\u0438\u043C\u0443\u043B\
  \u044E\u0432\u0430\u0442\u0438 \u0457\u0445 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u0430\u043D\u043D\u044F \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0456\
  \ \u0445\u0435\u0448\u0443\u0432\u0430\u043D\u043D\u044F."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
weight: 15
---

## Як це зробити:
C не має вбудованої підтримки асоціативних масивів, на відміну від деяких мов вищого рівня, але ви можете симулювати їх використання за допомогою структур і хешування. Нижче наведено спрощений приклад, який використовує комбінацію структури та простої хеш-функції для реалізації асоціативного масиву для зберігання та доступу до цілих чисел за рядковими ключами.

Спочатку визначте структуру для представлення окремої пари ключ-значення та іншу для представлення самого асоціативного масиву:

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

    printf("%d\n", find(&a, "key1")); // Вивід: 1
    printf("%d\n", find(&a, "key2")); // Вивід: 2

    return 0;
}
```

Цей приклад демонструє базові операції: ініціалізація асоціативного масиву, вставка пар ключ-значення та пошук значень за ключами. Зауважте, що цей код не обробляє колізії і призначений для навчальних цілей.

## Поглиблений Розгляд
Концепція асоціативних масивів існувала до мови C, але її низькорівнева природа не підтримує їх як вбудовані типи безпосередньо. Це спонукає до глибшого розуміння структур даних та алгоритмів, включаючи механізми хешування для ефективного відображення ключ-значення. Багато бібліотек та фреймворків мови C пропонують більш вдосконалені підходи до реалізації асоціативних масивів, такі як `GHashTable` від GLib, який забезпечує надійну реалізацію, включаючи обробку колізій, динамічне масштабування та підтримку довільних типів ключів та значень.

Поки ручне створення асоціативних масивів у C може здаватися обтяжливим порівняно з мовами з вбудованою підтримкою, воно пропонує незамінні уявлення про внутрішню роботу структур даних, загострюючи навички програміста в розв'язанні проблем та оптимізації. Однак, для продуктивного коду або більш складних застосунків, використання існуючих бібліотек, таких як GLib, часто є більш практичним і ефективним за часом підходом.
