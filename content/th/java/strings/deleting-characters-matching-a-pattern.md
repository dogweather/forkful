---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:16.838359-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Java, \u0E40\
  \u0E23\u0E32\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `String.replaceAll()`\
  \ \u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E1A regex pattern \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
lastmod: '2024-03-17T21:57:56.064694-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Java, \u0E40\u0E23\u0E32\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\
  \u0E40\u0E21\u0E18\u0E2D\u0E14 `String.replaceAll()` \u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A regex pattern \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E25\u0E1A\u0E2D\u0E31\
  \u0E01\u0E02\u0E23\u0E30 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\
  \u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
ใน Java, เรามักใช้เมธอด `String.replaceAll()` ร่วมกับ regex pattern เพื่อลบอักขระ นี่คือตัวอย่างอย่างรวดเร็ว:

```Java
public class PatternDeletionExample {
    public static void main(String[] args) {
        String originalString = "Hello, 123 World! This-is a test-string.";
        String pattern = "\\d|-"; // \d คือตัวเลข, - คือเครื่องหมายขีด

        String cleanedString = originalString.replaceAll(pattern, "");
        System.out.println(cleanedString); // แสดงผล: Hello,  World! This is a teststring.
    }
}
```
โค้ดนี้จะตัดตัวเลขและเครื่องหมายขีดออกเพื่อทำความสะอาดสตริงของเรา

## ค้นลึก
ในอดีต, คนๆหนึ่งได้จัดการกับสตริงโดยไม่มีเมธอดที่สะดวกและ regex พวกเขาทำมันด้วยวิธีที่ยากลำบาก, อักขระต่ออักขระ, ซึ่งเป็นเรื่องที่เจ็บปวด แล้ว regular expressions (regex) ก็ปรากฏขึ้น, และทุกอย่างก็ง่ายขึ้นมาก regex เป็นมาตรฐานการจับคู่แพทเทิร์นที่มีพลังในการประมวลผลข้อความ

แล้วทำไมต้องใช้ `replaceAll()`? มันเป็นส่วนหนึ่งของคลาส `String` ใน Java, และเนื่องจากสตริงมีอยู่ทุกที่, มันจึงกลายเป็นตัวเลือกแรกสำหรับการแก้ไขข้อความตามแพทเทิร์น มันรับพารามิเตอร์สองตัว: regex สำหรับแพทเทิร์นที่ต้องการกำจัดและสิ่งที่จะใส่แทน — ในกรณีของเรา, สตริงว่างเพื่อลบมัน

มีทางเลือกอื่นเช่นคลาส `Pattern` และ `Matcher` สำหรับงานที่ซับซ้อนมากกว่า ทั้งสองมีประโยชน์สำหรับงานที่ละเอียดอ่อนขึ้น เช่น การหาแพทเทิร์นโดยไม่ลบออก, หรือการแทนที่ในวิธีที่ซับซ้อนขึ้น

การทำงานขึ้นอยู่กับเอ็นจิน regex ของ Java, ซึ่งวิเคราะห์แพทเทิร์นและนำมาใช้กับสตริงเป้าหมาย เป็นภารกิจค้นหาและทำลายอักขระเล็กๆ—ค้นหาแพทเทิร์น, แล้วกำจัดมัน

## ดูเพิ่มเติม
- คลาส Java `Pattern`: [java.util.regex.Pattern](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- คลาส Java `Matcher`: [java.util.regex.Matcher](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Matcher.html)
- คู่มือการใช้งาน Regex: [Regular Expressions – User Guide](https://docs.oracle.com/javase/tutorial/essential/regex/)
- เมธอด `replaceAll()`: [java.lang.String#replaceAll](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
