---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:25.684130-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E08\u0E32\u0E01\u0E1B\
  \u0E25\u0E32\u0E22\u0E2A\u0E39\u0E48\u0E1B\u0E25\u0E32\u0E22\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E21\
  \u0E48 \u0E21\u0E31\u0E19\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\
  \u0E14\u0E40\u0E2D\u0E07 \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.072545-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E08\u0E32\u0E01\u0E1B\
  \u0E25\u0E32\u0E22\u0E2A\u0E39\u0E48\u0E1B\u0E25\u0E32\u0E22\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E21\
  \u0E48 \u0E21\u0E31\u0E19\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\
  \u0E14\u0E40\u0E2D\u0E07 \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25 \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\
  \u0E33\u0E40\u0E02\u0E49\u0E32\u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  ."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีการ:
นี่คือคำแนะนำเบื้องต้นในการต่อสตริงใน Java:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "John";
        String lastName = "Doe";
        
        // การใช้ operator บวก
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // ผลลัพธ์: John Doe
        
        // การใช้เมธอด concat()
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // ผลลัพธ์: John Doe
        
        // การใช้ StringBuilder สำหรับการต่อสตริงหลายครั้ง
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // ผลลัพธ์: John Doe
    }
}
```

## ลงลึก
การต่อสตริงดูเหมือนจะเรียบง่ายใช่ไหม? มันอยู่ใน Java ตั้งแต่เริ่มต้น และเรามีหลายวิธีในการทำมัน ในเวอร์ชัน Java เริ่มแรกใช้ StringBuilder เบื้องหลังการต่อสตริงด้วย `+` แล้วมาถึง Java 5 และเรื่องราวก็มีประสิทธิภาพมากขึ้นด้วยการนำเสนอ `StringJoiner` และการปรับปรุงเพิ่มเติมของคลาส `StringBuilder`

ตอนนี้ คุณอาจสงสัยว่าทำไมไม่ใช้ operator `+` เสมอไปถ้ามันเป็นเรื่องเดียวกัน? ปรากฎว่า `+` เหมาะสำหรับงานเร็วๆ กับสตริงขนาดเล็กหรือการต่อไม่กี่ครั้ง แต่ในขณะที่ทำงานอยู่เบื้องหลัง อาจทำให้เสียประสิทธิภาพถ้าคุณใช้มันในลูปที่มีการทำซ้ำจำนวนมากเนื่องจากมันสร้างวัตถุชั่วคราวก่อนที่จะได้สตริงเวอร์ชันสุดท้าย

ในกรณีที่มีการทำงานหนักๆ เข้ามา `StringBuilder` หรือ `StringBuffer` เป็นตัวเลือก โดยที่ `StringBuilder` มักจะเร็วกว่าเพราะไม่มีการเชื่อมต่อ—ทำให้มันไม่ปลอดภัยต่อเธรดแต่เร็วขึ้น `StringBuffer` เป็นตัวเลือกที่เก่ากว่า ปลอดภัยต่อเธรด มันช้าลงเนื่องจากความต้องการในการเชื่อมต่อ เลือกตามความต้องการเรื่องความปลอดภัยของเธรด 

สำหรับเมธอด `concat()` มันเป็นวิธีการที่ตรงไปตรงมาแต่ไม่ยืดหยุ่นเท่า `StringBuilder` ต้องการที่จะลูปและเพิ่มสตริงเพิ่มเติม? `concat()` ไม่ค่อยสะดวก

สำหรับ Java 8 และเวอร์ชันที่ใหม่กว่า เรายังมี `String.join()` ซึ่งค่อนข้างสะดวกสำหรับการรวมกลุ่มของสตริงที่มีตัวคั่น

## ดูเพิ่มเติม
- [เอกสารคลาส `String`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [เอกสารคลาส `StringBuilder`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [เอกสารคลาส `StringBuffer`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [การสอน Java ของ Oracle เกี่ยวกับ Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
