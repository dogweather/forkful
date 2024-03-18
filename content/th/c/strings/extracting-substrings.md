---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:10.550572-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E22\u0E48\u0E2D\u0E22 (substring) \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\
  \ C \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\
  \u0E35\u0E48\u0E40\u0E25\u0E47\u0E01\u0E01\u0E27\u0E48\u0E32\u0E08\u0E32\u0E01\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E02\u0E19\u0E32\u0E14\u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\
  \u0E22\u0E2D\u0E34\u0E07\u0E15\u0E32\u0E21\u0E40\u0E01\u0E13\u0E11\u0E4C\u0E17\u0E35\
  \u0E48\u0E23\u0E30\u0E1A\u0E38 \u0E40\u0E0A\u0E48\u0E19 \u0E15\u0E33\u0E41\u0E2B\
  \u0E19\u0E48\u0E07\u0E41\u0E25\u0E30\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u2026"
lastmod: '2024-03-17T21:57:56.675394-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E22\u0E48\u0E2D\u0E22 (substring) \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\
  \ C \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\
  \u0E35\u0E48\u0E40\u0E25\u0E47\u0E01\u0E01\u0E27\u0E48\u0E32\u0E08\u0E32\u0E01\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E02\u0E19\u0E32\u0E14\u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\
  \u0E22\u0E2D\u0E34\u0E07\u0E15\u0E32\u0E21\u0E40\u0E01\u0E13\u0E11\u0E4C\u0E17\u0E35\
  \u0E48\u0E23\u0E30\u0E1A\u0E38 \u0E40\u0E0A\u0E48\u0E19 \u0E15\u0E33\u0E41\u0E2B\
  \u0E19\u0E48\u0E07\u0E41\u0E25\u0E30\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การสร้างสตริงย่อย (substring) ในภาษา C เกี่ยวข้องกับการสร้างสตริงที่เล็กกว่าจากสตริงขนาดใหญ่โดยอิงตามเกณฑ์ที่ระบุ เช่น ตำแหน่งและความยาว โปรแกรมเมอร์มักจะทำงานนี้เพื่อการแยกวิเคราะห์ข้อความ การประมวลผลข้อมูล หรือการตรวจสอบข้อมูลที่ป้อน เป็นทักษะสำคัญในการจัดการและวิเคราะห์ข้อมูลข้อความอย่างมีประสิทธิภาพ

## วิธีการ:

ไม่เหมือนกับภาษาระดับสูงอื่น ๆ ที่มีวิธีการในตัวสำหรับการสร้างสตริงย่อย C ต้องใช้วิธีการที่มากขึ้นด้วยตัวมันเองในการใช้ฟังก์ชันการจัดการสตริง นี่คือวิธีการสร้างสตริงย่อยใน C อย่างมีประสิทธิภาพ:

### ตัวอย่างที่ 1: ใช้ `strncpy`

```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hello, World!";
    char buffer[20];

    // สร้าง "World" จาก "Hello, World!"
    strncpy(buffer, text + 7, 5);
    buffer[5] = '\0'; // รับรองการสิ้นสุดด้วย null

    printf("สตริงย่อยที่สร้าง: %s\n", buffer);
    // ผลผลิต: สตริงย่อยที่สร้าง: World
    return 0;
}
```

### ตัวอย่างที่ 2: สร้างฟังก์ชัน

สำหรับการใช้งานที่ซ้ำๆ ฟังก์ชันที่ถูกสร้างขึ้นเพื่อสร้างสตริงย่อยอาจจะมีประสิทธิภาพมากขึ้น:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *source, int from, int n, char *target) {
    strncpy(target, source + from, n);
    target[n] = '\0'; // รับรองการสิ้นสุดด้วย null
}

int main() {
    char text[] = "Programming in C";
    char buffer[50];

    extractSubstring(text, 0, 11, buffer);
    printf("สตริงย่อยที่สร้าง: %s\n", buffer);
    // ผลผลิต: สตริงย่อยที่สร้าง: Programming
    return 0;
}
```

## ลงลึก

การสร้างสตริงย่อยใน C ส่วนใหญ่จัดการผ่านการควบคุมตัวชี้และการจัดการหน่วยความจำอย่างรอบคอบ สะท้อนถึงวิธีการระดับล่างในการจัดการข้อมูลของภาษา วิธีการนี้ย้อนกลับไปในยุคแรกๆ ของการเขียนโปรแกรม C เมื่อการจัดการทรัพยากรอย่างมีประสิทธิภาพเป็นสิ่งสำคัญเนื่องจากกำลังคอมพิวเตอร์ที่จำกัด ขณะที่การไม่มีฟังก์ชันสตริงย่อยในตัวอาจดูเหมือนเป็นการละเลย แต่มันยกย่องปรัชญาของภาษา C ที่ให้โปรแกรมเมอร์มีการควบคุมการจัดการหน่วยความจำอย่างเต็มที่ ซึ่งมักจะนำไปสู่โค้ดที่ถูกปรับให้เหมาะสมแต่ซับซ้อนมากขึ้น

ในโลกของการเขียนโปรแกรมยุคใหม่ ภาษาอย่าง Python และ JavaScript นำเสนอวิธีการในตัวสำหรับการสร้างสตริงย่อย เช่น `slice()` หรือการตัดสตริงโดยใช้ดัชนี ภาษาเหล่านี้จัดการหน่วยความจำเบื้องหลัง ให้ความสำคัญกับความง่ายดายในการใช้งานและความสามารถในการอ่านอย่างใดอย่างหนึ่ง 

สำหรับโปรแกรมเมอร์ C การเข้าใจการคำนวณตัวชี้และการจัดสรรหน่วยความจำเป็นสิ่งสำคัญสำหรับงานเช่นการสร้างสตริงย่อย แม้วิธีการนี้จะต้องการความเข้าใจลึกซึ้งเกี่ยวกับวิธีการแทนและจัดการสตริงในหน่วยความจำ แต่มันก็เสนอการควบคุมและประสิทธิภาพอย่างไม่มีใครเทียบได้ ซึ่งเป็นลักษณะเด่นของการเขียนโปรแกรม C ที่เก็บไว้ความเกี่ยวข้องในการใช้งานที่ต้องการประสิทธิภาพสูงมาหลายทศวรรษ อย่างไรก็ตาม สำหรับผู้ที่ทำงานกับแอปพลิเคชันระดับสูงที่การจัดการหน่วยความจำโดยตรงน้อยความกังวล ภาษาที่มีฟังก์ชันสตริงย่อยในตัวอาจเสนอวิธีการที่ง่ายและลดความผิดพลาดได้มากกว่า