---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:25.510606-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 HTML \u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E16\u0E39\u0E01\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\
  \u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.519456-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 HTML \u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E16\u0E39\u0E01\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\
  \u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## อะไรและทำไม?
การแยกวิเคราะห์ HTML หมายถึงการทำให้เนื้อหา HTML สามารถถูกโปรแกรมเข้าใจและจัดการได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อดึงข้อมูล, การจัดการเนื้อหา, หรือการรวมการเว็บ scraping เข้ากับแอปพลิเคชั่นของพวกเขา

## วิธีการ:
C++ ไม่มีความสามารถในการแยกวิเคราะห์ HTML มาให้เบื้องต้น คุณจะต้องใช้ไลบรารีเช่น Gumbo-parser โดย Google หรืออย่างอื่นที่คล้ายคลึงกัน นี่คือตัวอย่างอย่างรวดเร็วโดยใช้ Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

ผลลัพธ์ตัวอย่าง:
```
https://example.com
```

## ศึกษาลึกลงไป
การแยกวิเคราะห์ HTML ไม่เคยเป็นเรื่องง่ายใน C++ ในอดีต โปรแกรมเมอร์จะใช้ regex หรือ parser ที่เขียนด้วยมือซึ่งทั้งคู่มีโอกาสเกิดข้อผิดพลาดและใช้งานยาก ในปัจจุบัน ไลบรารีที่มีความเขี้ยวเล็บเช่น Gumbo-parser จัดการกับความซับซ้อนของการแยกวิเคราะห์ ทำให้ง่ายและมีความน่าเชื่อถือมากขึ้น

ทางเลือกอื่น ๆ รวมถึง Tidy, MyHTML, หรือแม้กระทั่งการรวม C++ กับ BeautifulSoup ของ Python ผ่านฟังก์ชัน `system` ของ C++ หรือตัวถอดรหัสที่ฝังอยู่

ในด้านการใช้งาน, ไลบรารีเหล่านี้แปลง HTML ไปเป็นต้นไม้ของ Document Object Model (DOM) การเดินทางและจัดการ DOM อนุญาตให้ผู้ใช้สามารถดึงและทำงานกับข้อมูลได้เช่นที่แสดงในส่วนวิธีการ

## ดูเพิ่มเติม
- [Gumbo-parser GitHub repository](https://github.com/google/gumbo-parser)
- [รายการของไลบรารีการแยกวิเคราะห์ HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [การทำงานร่วมกันระหว่าง C++ และ Python](https://docs.python.org/3/extending/embedding.html)
