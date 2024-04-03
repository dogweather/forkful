---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:38.583936-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19\
  \ Java \u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E31\u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E14\u0E34\u0E21\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E43\u0E2B\u0E21\u0E48 - \u0E04\u0E34\u0E14\u0E27\u0E48\u0E32\u0E21\
  \u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E2D\u0E07\u0E02\u0E32\u0E27\u0E14\u0E34\
  \u0E08\u0E34\u0E17\u0E31\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.065621-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19\
  \ Java \u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E31\u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E14\u0E34\u0E21\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E43\u0E2B\u0E21\u0E48 - \u0E04\u0E34\u0E14\u0E27\u0E48\u0E32\u0E21\
  \u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E2D\u0E07\u0E02\u0E32\u0E27\u0E14\u0E34\
  \u0E08\u0E34\u0E17\u0E31\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E1B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32, \u0E2B\u0E23\
  \u0E37\u0E2D\u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  ."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## อะไร & ทำไม?

การค้นหาและแทนที่ข้อความใน Java เขียนทับสตริงเดิมด้วยตัวอักษรใหม่ - คิดว่ามันเป็นของขาวดิจิทัล โปรแกรมเมอร์มักใช้วิธีนี้เพื่อทำความสะอาดข้อมูล, ปรับการตั้งค่า, หรือแก้ไขข้อความ

## วิธีการ:

การค้นหาและแทนที่ใน Java ทำได้ง่ายด้วยคลาส `String` และเมธอด `replace()` นี่คือวิธีการทำ:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String originalText = "The quick brown fox jumps over the lazy dog";
        String modifiedText = originalText.replace("lazy", "energetic");
        
        System.out.println("ก่อน: " + originalText);
        System.out.println("หลัง: " + modifiedText);
    }
}
```

ผลลัพธ์:
```
ก่อน: The quick brown fox jumps over the lazy dog
หลัง: The quick brown fox jumps over the energetic dog
```

ตอนนี้, สำหรับรูปแบบหรือการแทนที่ที่มากกว่านั้น, `Pattern` และ `Matcher` มาเข้าร่วม:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String originalText = "There are 31,536,000 seconds in 365 days.";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(originalText);
        String modifiedText = matcher.replaceAll("#");
        
        System.out.println("ก่อน: " + originalText);
        System.out.println("หลัง: " + modifiedText);        
    }
}
```

ผลลัพธ์:
```
ก่อน: There are 31,536,000 seconds in 365 days.
หลัง: There are # seconds in # days.
```

## ลงลึก:

เมธอด `replace()` ติดตามต้นกำเนิดมาจากวันแรกๆ ของ Java มันเป็นส่วนหนึ่งของคลาส `String` ที่ไม่สามารถเปลี่ยนแปลงได้, หมายความว่าทุกครั้งที่คุณใช้มัน, คุณกำลังสร้างสตริงใหม่ มีความเป็นมิตรต่อสิ่งแวดล้อมอย่างมาก, ไม่มีการสิ้นเปลืองสิ่งเก่า

แต่ `Pattern` และ `Matcher` มันคืออะไร, คุณถาม? คลาสเหล่านี้เป็นส่วนหนึ่งของ API สำหรับ regular expression (regex) ของ Java, ที่แนะนำใน Java 1.4 พวกมันเพิ่มความสามารถให้การค้นหาและการแทนที่, ช่วยให้คุณตรวจจับรูปแบบที่ซับซ้อนและแก้ไขข้อความได้แบบไดนามิก เหมือนกับการใช้มีดผ่าตัดแทนค้อนใหญ่

นอกจากนี้, ยังมี `replaceAll()` และ `replaceFirst()`, สองเมธอดของคลาส `Matcher` ที่ปรับแต่งการเปลี่ยนแปลงข้อความของคุณ, แทนที่ทุกครั้งหรือเฉพาะครั้งแรกที่ตรงกัน

ตัวเลือกอื่นคือการใช้คลาส `StringBuffer` หรือ `StringBuilder` เมื่อคุณกำลังจัดการกับการแก้ไขจำนวนมาก เพราะไม่เหมือน `String`, บัฟเฟอร์เหล่านี้สามารถเปลี่ยนแปลงได้

## ดูเพิ่มเติมที่:

- [เอกสาร String ของ Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [เอกสาร Pattern ของ Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [เอกสาร Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [บทเรียนสำหรับ Regular Expressions](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

สำหรับการฝึกฝนมือถือมากขึ้น, ลองดูที่ RegexOne (https://regexone.com), เป็นแหล่งที่ยอดเยี่ยมในการพัฒนาทักษะ regex ของคุณ
