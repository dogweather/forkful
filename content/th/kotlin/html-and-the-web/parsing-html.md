---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:30.412515-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33 HTML \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\
  \u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E44\u0E14\u0E49 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E43\
  \u0E0A\u0E49\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33 HTML \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.172931-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33 HTML \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\
  \u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E44\u0E14\u0E49 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E43\
  \u0E0A\u0E49\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33 HTML \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E2D\u0E31\
  \u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\u0E15\
  \u0E2D\u0E1A\u0E1A\u0E19\u0E40\u0E27\u0E47\u0E1A, \u0E2B\u0E23\u0E37\u0E2D\u0E22\
  \u0E49\u0E32\u0E22\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E23\u0E30\u0E2B\u0E27\
  \u0E48\u0E32\u0E07\u0E23\u0E30\u0E1A\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
Kotlin ทำให้การแยกคำ HTML เป็นเรื่องง่ายด้วยไลบรารีเช่น Jsoup นี่คือวิธีทำ:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>หน้าตัวอย่าง</title></head><body><p>นี่เป็นการทดสอบ.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("หัวเรื่อง: $title")  // ผลลัพธ์: หัวเรื่อง: หน้าตัวอย่าง

    val pText = doc.select("p").first()?.text()
    println("ย่อหน้า: $pText")  // ผลลัพธ์: ย่อหน้า: นี่เป็นการทดสอบ.
}
```

เราดึงหัวเรื่องและข้อความในย่อหน้า, เพียงแค่เริ่มต้นสำรวจสิ่งที่ Jsoup สามารถทำได้

## การศึกษาลึก:
ก่อน Kotlin, Java เป็นภาษาหลักสำหรับงานนี้ แต่มักจะดูไม่คล่องตัว, Jsoup เปลี่ยนเกมด้วยการนำเสนอวิธีการที่คล้ายกับ jQuery การแยกคำ HTML ไม่ได้จำกัดอยู่เฉพาะกับ Jsoup เท่านั้น; มีไลบรารีอื่นๆ เช่น HtmlUnit หรือแม้แต่ regex (แม้ว่าจะไม่แนะนำ) ก็มีให้ใช้งาน ด้วย Jsoup, คุณสามารถแน่ใจได้ว่าการแยกคำของคุณจะเคารพต่อโครงสร้างเอกสาร มันใช้โมเดล DOM, ซึ่งช่วยให้สามารถเลือกและจัดการกับองค์ประกอบได้ มันยืดหยุ่นดีเช่นกัน—สามารถแยกคำ HTML ที่รกที่สุดได้

## ดูเพิ่มเติม:
ศึกษาเพิ่มเติมเกี่ยวกับ Jsoup:

- เอกสารอย่างเป็นทางการของ Jsoup: https://jsoup.org/
- หนังสือ "Kotlin สำหรับนักพัฒนา Android": https://antonioleiva.com/kotlin-android-developers-book/
- เว็บไซต์ภาษาโปรแกรม Kotlin อย่างเป็นทางการ: https://kotlinlang.org/

สำหรับการสนทนาและบทเรียนสอนเพิ่มเติมเกี่ยวกับการคัดลอกเว็บและการแยกคำ:

- การคัดลอกเว็บด้วย Kotlin และ Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- การแยกคำ HTML บน Android ด้วย Kotlin และ Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
