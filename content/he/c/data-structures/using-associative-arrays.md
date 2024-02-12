---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-02-03T18:11:23.383165-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, הידועים בשפות אחרות כמפות או מילונים, הם זוגות מפתח-ערך המשמשים לחיפוש ולהתערבות בנתונים בצורה יעילה. בניגוד למערכים מסורתיים המשתמשים באינדקסים מסוג מספר שלם, מערכים אסוציאטיביים משתמשים במפתחות, מה שהופך את הגישה לנתונים ליותר אינטואיטיבית וגמישה עבור מתכנתים.

## איך ל:

בשפת C אין תמיכה מובנית למערכים אסוציאטיביים כמו בחלק מהשפות ברמה גבוהה יותר, אך ניתן לסימולציה שלהם באמצעות מבנים ואלגוריתם גיבוב. להלן דוגמה פשטנית בשימוש במבנה ובפונקציית גיבוב פשוטה ליישום מערך אסוציאטיבי לאחסון וגישה למספרים שלמים על ידי מפתחות מחרוזת.

תחילה, הגדר מבנים לייצוג זוג מפתח-ערך בודד ומבנה נוסף לייצוג המערך האסוציאטיבי עצמו:

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

    printf("%d\n", find(&a, "key1")); // פלט: 1
    printf("%d\n", find(&a, "key2")); // פלט: 2

    return 0;
}
```

הדוגמה הזו מדגימה פעולות בסיסיות: אתחול מערך אסוציאטיבי, הכנסת זוגות מפתח-ערך, ומציאת ערכים על פי מפתחות. שימו לב שהקוד הזה חסר טיפול בהתנגשויות ומיועד למטרות הדרכה.

## עיון עמוק

המושג של מערכים אסוציאטיביים קדם לשפת C, אך הטבע הנמוך-רמה של השפה אינו תומך בהם ישירות כטיפוסים מובנים. זה מעודד הבנה עמוקה יותר של מבני נתונים ואלגוריתמים, כולל מנגנוני גיבוב למיפוי מפתח-ערך בצורה יעילה. ספריות ופריימוורקים רבים בשפת C מציעים גישות יותר מתוחכמות ליישום מערכים אסוציאטיביים, כמו `GHashTable` של GLib, המספקת מימוש עמיד עם טיפול בהתנגשויות, שינוי גודל דינמי, ותמיכה בטיפוסי מפתח וערך שרירותיים.

בעוד שבניית מערכים אסוציאטיביים ב-C ידנית עשויה להיתפס כמסורבלת בהשוואה לשפות עם תמיכה מובנית, היא מציעה הזדמנות יקרת ערך להבנה של ההיגיון הפנימי של מבני נתונים, ומחדדת את כישורי הפתרון בעיות והאופטימיזציה של המתכנת. עם זאת, לקוד פרודקשן או ליישומים מורכבים יותר, להשתמש בספריות קיימות כמו GLib הוא לעיתים קרובות גישה מעשית ויעילה יותר מבחינת זמן.
