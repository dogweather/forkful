---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:32.131550-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Arduino \u0E44\u0E21\u0E48\
  \u0E44\u0E14\u0E49\u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\
  \u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E18\u0E23\u0E23\u0E21\u0E0A\u0E32\u0E15\u0E34\
  \u0E43\u0E19\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\
  \u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E23\
  \u0E30\u0E14\u0E31\u0E1A\u0E2A\u0E39\u0E07 \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\
  \u0E01\u0E47\u0E15\u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.467382-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E23\u0E2D\u0E07\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E18\u0E23\
  \u0E23\u0E21\u0E0A\u0E32\u0E15\u0E34\u0E43\u0E19\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\
  \u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E23\u0E30\u0E14\u0E31\u0E1A\u0E2A\u0E39\u0E07 \u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21, \u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\
  \u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E02\u0E2D\u0E07\u0E15\u0E31\
  \u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E2B\u0E23\u0E37\u0E2D\u0E43\u0E0A\u0E49\u0E04\
  \u0E25\u0E32\u0E2A `String` \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E40\u0E21\u0E18\
  \u0E2D\u0E14 `replace()`."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
Arduino ไม่ได้รองรับการค้นหาและแทนที่สตริงโดยธรรมชาติในลักษณะเดียวกับภาษาโปรแกรมระดับสูง อย่างไรก็ตาม, คุณสามารถทำงานกับอาร์เรย์ของตัวอักษรหรือใช้คลาส `String` ซึ่งมีเมธอด `replace()`. ทั้งนี้, วิธีแรกจะประหยัดหน่วยความจำ แต่วิธีที่สองจะง่ายกว่า ลองมุ่งเน้นที่คลาส `String` เพื่อความชัดเจน

```Arduino
void setup() {
  Serial.begin(9600);
  String text = "I like apples and apples are great!";
  text.replace("apples", "oranges");
  Serial.println(text);
}

void loop() {
  // ที่นี่ไม่ต้องทำอะไร
}
```

ผลลัพธ์ตัวอย่าง:
```
I like oranges and oranges are great!
```

## การศึกษาเจาะลึก
ในสมัยก่อน งานด้านการดำเนินการกับสตริงบนไมโครคอนโทรลเลอร์เป็นเรื่องที่หาได้ยาก — หน่วยความจำมีจำกัดและแอพพลิเคชั่นง่ายกว่า แต่ในปัจจุบัน ด้วยโปรเจกต์ที่ซับซ้อนขึ้นและพื้นที่หน่วยความจำที่มากขึ้น (ขอบคุณการพัฒนาเทคโนโลยีไมโครคอนโทรลเลอร์) ยูทิลิตี้เหล่านี้จึงกลายเป็นมาตรฐาน

หากคุณไม่ต้องการใช้คลาส `String` เนื่องจากการใช้หน่วยความจำแบบไดนามิกซึ่งอาจทำให้เกิดการแบ่งส่วน คุณยังคงสามารถค้นหาและแทนที่ในสตริงแบบ C (อาร์เรย์ของตัวอักษรที่จบด้วย null) โดยใช้ฟังก์ชันเช่น `strchr()`, `strstr()`, และการคัดลอกหรือการแทนที่ด้วยลูปแบบด้วยมือ วิธีนี้อาจมีความยุ่งยากมากขึ้นแต่ให้ความควบคุมเรื่องหน่วยความจำได้ดีขึ้น

ตัวอย่างเช่น, วิธีอื่นในการแทนที่สับสตริงอาจดูเช่นนี้:

```Arduino
void replaceSubstring(char *input, const char *search, const char *replace) {
  char buffer[100];
  char *p;

  // 'strstr' ตรวจสอบว่า 'search' เป็นส่วนหนึ่งของ 'input' หรือไม่
  if (!(p = strstr(input, search))) return;

  // คัดลอกจนถึงจุดที่พบ 'search'
  strncpy(buffer, input, p - input);
  buffer[p - input] = '\0';

  // ต่อท้าย 'replace' และส่วนที่เหลือของ 'input' หลังจาก 'search'
  sprintf(buffer+(p - input), "%s%s", replace, p + strlen(search));

  // แสดงผลลัพธ์
  strcpy(input, buffer);
}

void setup() {
  Serial.begin(9600);
  char text[] = "I like apples and apples are great!";
  replaceSubstring(text, "apples", "oranges");
  Serial.println(text);
}

void loop() {
  // ที่นี่ก็ยังไม่ต้องทำอะไร
}
```

ผลลัพธ์ตัวอย่าง:
```
I like oranges and oranges are great!
```

## ดูเพิ่มเติม
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Reference: String Replace Function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Cplusplus.com: C String Functions](http://www.cplusplus.com/reference/cstring/)
