---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:13.841017-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: C \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\
  \u0E2D\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E14\u0E31\u0E07\
  \u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\
  \u0E49\u0E07\u0E08\u0E36\u0E07\u0E15\u0E49\u0E2D\u0E07\u0E2B\u0E31\u0E19\u0E44\u0E1B\
  \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `strptime` \u0E17\
  \u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35 `<time.h>`\u2026"
lastmod: '2024-04-05T21:54:02.705420-06:00'
model: gpt-4-0125-preview
summary: "C \u0E44\u0E21\u0E48\u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\
  \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2D\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\
  \u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07 \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\u0E1A\
  \u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\u0E08\u0E36\u0E07\u0E15\u0E49\u0E2D\
  \u0E07\u0E2B\u0E31\u0E19\u0E44\u0E1B\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19 `strptime` \u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `<time.h>` \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A POSIX \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19\u0E19\u0E35\u0E49\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E23\u0E32\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E30\u0E1A\u0E38\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48\u0E04\u0E32\u0E14\u0E2B\u0E27\u0E31\u0E07\u0E02\u0E2D\u0E07\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E41\u0E25\u0E30\
  \u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19 `struct tm` \u0E0B\u0E36\u0E48\u0E07\u0E41\u0E2A\
  \u0E14\u0E07\u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\
  \u0E40\u0E27\u0E25\u0E32\u0E02\u0E2D\u0E07\u0E1B\u0E0F\u0E34\u0E17\u0E34\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E22\u0E01\u0E2D\u0E2D\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\
  \u0E27\u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E02\
  \u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `strptime`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\
  \u0E15\u0E23\u0E34\u0E07."
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:
C ไม่มีวิธีที่ในตัวเองในการแยกวิเคราะห์วันที่จากสตริงโดยตรง ดังนั้นเราบ่อยครั้งจึงต้องหันไปใช้ฟังก์ชัน `strptime` ที่มีอยู่ในไลบรารี `<time.h>` สำหรับระบบ POSIX ฟังก์ชันนี้ทำให้เราสามารถระบุรูปแบบที่คาดหวังของสตริงนำเข้าและแยกวิเคราะห์เข้าไปใน `struct tm` ซึ่งแสดงถึงวันที่และเวลาของปฏิทินที่แยกออกเป็นส่วนประกอบ

นี่คือตัวอย่างง่ายๆ ของวิธีการใช้ `strptime` เพื่อแยกวิเคราะห์วันที่จากสตริง:

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // การแยกวิเคราะห์สตริงวันที่เข้าไปใน struct tm
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("Failed to parse date.\n");
    } else {
        // การใช้ strftime เพื่อพิมพ์วันที่ในรูปแบบที่อ่านง่าย
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("Parsed date: %s\n", buf);
    }

    return 0;
}
```

ผลลัพธ์จากโปรแกรมนี้จะเป็น:

```
Parsed date: Saturday, April 01, 2023
```

สิ่งสำคัญคือต้องจัดการกับข้อผิดพลาดที่อาจเกิดขึ้น เช่น `strptime` ที่ล้มเหลวในการจับคู่รูปแบบหรือพบกับข้อมูลนำเข้าที่ไม่คาดคิด

## การศึกษาลึก
ฟังก์ชัน `strptime` แม้จะมีประสิทธิภาพ แต่ไม่ได้เป็นส่วนหนึ่งของไลบรารีมาตรฐานของภาษา C และพบได้ส่วนใหญ่ในระบบที่เข้ากันได้กับ POSIX เช่น Linux และ UNIX ข้อจำกัดนี้หมายความว่าโปรแกรมที่พึ่งพา `strptime` สำหรับการแยกวิเคราะห์วันที่จากสตริงอาจไม่สามารถใช้งานได้กับระบบที่ไม่ใช่ POSIX เช่น Windows โดยไม่ต้องมีชั้นความเข้ากันได้เพิ่มเติมหรือไลบรารี

ในอดีตการจัดการวันที่และเวลาในภาษา C ต้องใช้การจัดการและความรอบคอบมากมายโดยเฉพาะเมื่อพิจารณาถึง locales และเขตเวลาที่แตกต่างกัน ทางเลือกและส่วนขยายสมัยใหม่ของภาษา C เช่นไลบรารี `<chrono>` ของภาษา C++ และไลบรารีของบุคคลที่สามเช่นไลบรารีวันที่ของ Howard Hinnant สำหรับภาษา C++ นำเสนอโซลูชันที่มีความแข็งแกร่งมากขึ้นสำหรับการจัดการวันที่และเวลา รวมถึงการแยกวิเคราะห์ ไลบรารีเหล่านี้มักจะให้การสนับสนุนที่ดีกว่าสำหรับรูปแบบวันที่เขตเวลาและกลไกการจัดการข้อผิดพลาดที่หลากหลาย ทำให้พวกเขาเป็นทางเลือกที่น่าสนใจสำหรับโปรเจคใหม่ที่ต้องการความสามารถในการจัดการวันที่และเวลาอย่างกว้างขวาง

อย่างไรก็ตาม การเข้าใจวิธีการแยกวิเคราะห์วันที่จากสตริงในภาษา C อาจเป็นประโยชน์ เฉพาะเมื่อทำงานกับหรือดูแลโครงการที่ต้องการความเข้ากันได้กับระบบที่เครื่องมือสมัยใหม่เหล่านี้ไม่พร้อมใช้งานหรือเมื่อทำงานภายใต้ข้อจำกัดของสภาพแวดล้อมการโปรแกรมภาษา C ที่เข้มงวด
