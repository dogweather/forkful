---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:40.036054-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E2B\u0E49\u0E43\
  \u0E0A\u0E49 Jsoup, \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A HTML\
  \ \u0E43\u0E19\u0E42\u0E25\u0E01\u0E08\u0E23\u0E34\u0E07 \u0E02\u0E31\u0E49\u0E19\
  \u0E41\u0E23\u0E01, \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E04\u0E27\u0E32\u0E21\u0E02\
  \u0E36\u0E49\u0E19\u0E15\u0E48\u0E2D\u0E14\u0E31\u0E07\u0E19\u0E35\u0E49."
lastmod: '2024-03-17T21:57:56.079046-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49 Jsoup, \u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E17\u0E35\u0E48\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\
  \u0E4C\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E01\u0E31\u0E1A HTML \u0E43\u0E19\u0E42\u0E25\u0E01\u0E08\u0E23\u0E34\u0E07\
  \ \u0E02\u0E31\u0E49\u0E19\u0E41\u0E23\u0E01, \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E04\
  \u0E27\u0E32\u0E21\u0E02\u0E36\u0E49\u0E19\u0E15\u0E48\u0E2D\u0E14\u0E31\u0E07\u0E19\
  \u0E35\u0E49."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
ให้ใช้ Jsoup, ไลบรารีที่มีประโยชน์สำหรับการทำงานกับ HTML ในโลกจริง ขั้นแรก, เพิ่มความขึ้นต่อดังนี้:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

ตอนนี้เรามาสู่ส่วนที่สนุก นี่คือวิธีการดึงชื่อหน้าเว็บและพิมพ์ออกมา:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Title: " + title);
    }
}
```

ผลลัพธ์:

```
Title: Example Domain
```

เราจะดึงลิงก์ทั้งหมดได้อย่างไร?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... ภายใน main หรือวิธีการอื่น
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## การดำดิ่งลึก
เมื่อก่อน HTML ถูกจัดการด้วยรูปแบบ regex, วิธีการที่มีความผิดพลาดสูงและน่าสยดสยองสำหรับเอกสารที่ซับซ้อน Jsoup เข้ามาในปลายทศวรรษ 2000s, ให้การเข้าถึงแบบ jQuery สำหรับ Java เพื่อการแยกวิเคราะห์, การเดินทางผ่าน, และการจัดการ HTML

Jsoup ไม่ใช่ตัวเลือกเดียวที่มี มี HtmlUnit สำหรับการทดสอบแอปพลิเคชันเว็บแบบเต็มรูปแบบพร้อมการสนับสนุน JavaScript, แต่มันหนักและซับซ้อนกว่า สำหรับงานเบาๆ, Apache Commons Validator เป็นตัวเลือกที่ดีเพื่อการดึงลิงก์เท่านั้น

ภายใต้ฝาครอบ, Jsoup ใช้ DOM parser, ซึ่งทำแบบจำลองทั้งเอกสารในหน่วยความจำเป็นต้นไม้ วิธีนี้ทำให้การเลือกและนำทางโครงสร้าง HTML เป็นเรื่องง่าย แถมยังยืดหยุ่นกับ HTML ที่ไม่เรียบร้อย, แก้ไขปัญหาทันทีเพื่อให้มั่นใจการวิเคราะห์ที่เข้มแข็ง

จำไว้ว่า, เมื่อดึงข้อมูล, ตรวจสอบไฟล์ `robots.txt` และข้อกำหนดการให้บริการของเว็บไซต์เสมอ เพื่อหลีกเลี่ยงปัญหาทางกฎหมายหรือการถูกแบน IP

## ดูเพิ่มเติม
- Jsoup เอกสารอย่างเป็นทางการ: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
