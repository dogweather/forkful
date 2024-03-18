---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:36.957914-06:00
description: "\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E41\u0E1A\u0E1A\u0E1C\
  \u0E39\u0E01\u0E1E\u0E31\u0E19 (Associative arrays) \u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E37\u0E48\u0E19\
  \u0E46 \u0E27\u0E48\u0E32 maps \u0E2B\u0E23\u0E37\u0E2D dictionaries \u0E04\u0E37\
  \u0E2D\u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\
  \u0E04\u0E48\u0E32 (key-value pairs)\u2026"
lastmod: '2024-03-17T21:57:56.679223-06:00'
model: gpt-4-0125-preview
summary: "\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E41\u0E1A\u0E1A\u0E1C\
  \u0E39\u0E01\u0E1E\u0E31\u0E19 (Associative arrays) \u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E37\u0E48\u0E19\
  \u0E46 \u0E27\u0E48\u0E32 maps \u0E2B\u0E23\u0E37\u0E2D dictionaries \u0E04\u0E37\
  \u0E2D\u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\
  \u0E04\u0E48\u0E32 (key-value pairs)\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

อาร์เรย์แบบผูกพัน (Associative arrays) ที่รู้จักในภาษาอื่นๆ ว่า maps หรือ dictionaries คือคู่ของคีย์และค่า (key-value pairs) ที่ใช้สำหรับการค้นหาและจัดการข้อมูลอย่างมีประสิทธิภาพ ต่างจากอาร์เรย์แบบดั้งเดิมที่ใช้ดัชนีแบบจำนวนเต็ม, อาร์เรย์แบบผูกพันใช้คีย์ซึ่งทำให้การเข้าถึงข้อมูลสะดวกและยืดหยุ่นมากขึ้นสำหรับโปรแกรมเมอร์

## วิธีการ:

ภาษา C ไม่มีการสนับสนุนอาร์เรย์แบบผูกพันอย่างในตัวเหมือนภาษาระดับสูงอื่นๆ แต่คุณสามารถจำลองได้โดยใช้โครงสร้างและการแฮช ด้านล่างเป็นตัวอย่างง่ายๆ โดยใช้การรวมกันของ struct และฟังก์ชันแฮชที่ง่ายเพื่อสร้างอาร์เรย์แบบผูกพันสำหรับการจัดเก็บและเข้าถึงเลขจำนวนเต็มโดยใช้คีย์แบบสตริง

ก่อนอื่น, กำหนดโครงสร้างเพื่อแทนคู่คีย์-ค่าหนึ่งคู่และอีกอันหนึ่งเพื่อแทนอาร์เรย์แบบผูกพันเอง:

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

    printf("%d\n", find(&a, "key1")); // ผลลัพธ์: 1
    printf("%d\n", find(&a, "key2")); // ผลลัพธ์: 2

    return 0;
}
```

ตัวอย่างนี้สาธิตการดำเนินการพื้นฐาน: เริ่มต้นอาร์เรย์แบบผูกพัน, การแทรกคู่คีย์-ค่า, และหาค่าโดยใช้คีย์ โปรดทราบว่าโค้ดนี้ขาดการจัดการการชนกันและมีจุดประสงค์เพื่อการศึกษา

## ศึกษาลึกซึ้ง

แนวคิดของอาร์เรย์แบบผูกพันมีมาก่อนภาษา C, แต่ลักษณะที่เป็นภาษาระดับต่ำของมันไม่ให้การสนับสนุนโดยตรงเป็นประเภทในตัว นี่ทำให้เกิดการเข้าใจลึกซึ้งยิ่งขึ้นเกี่ยวกับโครงสร้างข้อมูลและอัลกอริทึม รวมถึงกลไกแฮชสำหรับการแมปคีย์-ค่าอย่างมีประสิทธิภาพ หลายไลบรารีและเฟรมเวิร์กของภาษา C นำเสนอวิธีการที่สมบูรณ์ยิ่งขึ้นสำหรับการสร้างอาร์เรย์แบบผูกพัน เช่น `GHashTable` ของ GLib ซึ่งให้การสนับสนุนการจัดการการชนกัน, การปรับขนาดไดนามิก, และสนับสนุนประเภทคีย์และค่าที่หลากหลาย

แม้ว่าการสร้างอาร์เรย์แบบผูกพันด้วยตัวเองในภาษา C อาจดูเหมือนลำบากเมื่อเปรียบเทียบกับภาษาที่มีการสนับสนุนในตัว แต่สิ่งนี้ให้ความเข้าใจอย่างลึกซึ้งเกี่ยวกับกลไกภายในของโครงสร้างข้อมูล ช่วยให้นักโปรแกรมเมอร์มีทักษะทางการแก้ปัญหาและการปรับปรุงประสิทธิภาพที่ดียิ่งขึ้น อย่างไรก็ดี, สำหรับโค้ดการผลิตหรือโปรแกรมที่ซับซ้อนมากขึ้น, การใช้ไลบรารีที่มีอยู่เช่น GLib มักเป็นวิธีที่ปฏิบัติและประหยัดเวลามากขึ้น
