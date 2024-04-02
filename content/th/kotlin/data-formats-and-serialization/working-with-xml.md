---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:16.396857-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C, \u0E2A\u0E23\u0E49\u0E32\u0E07 \u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 XML - \u0E20\u0E32\u0E29\u0E32\u0E21\
  \u0E32\u0E23\u0E4C\u0E01\u0E2D\u0E31\u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.207191-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C,\
  \ \u0E2A\u0E23\u0E49\u0E32\u0E07 \u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 XML - \u0E20\u0E32\u0E29\u0E32\u0E21\u0E32\
  \u0E23\u0E4C\u0E01\u0E2D\u0E31\u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\
  \u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## คืออะไร & ทำไม
การทำงานกับ XML เกี่ยวข้องกับการแยกวิเคราะห์, สร้าง และจัดการเอกสาร XML - ภาษามาร์กอัปสำหรับการจัดเก็บและการส่งข้อมูล โปรแกรมเมอร์ทำเช่นนี้เพราะมีหลายระบบที่ยังใช้ข้อมูลในรูปแบบ XML และมันจำเป็นสำหรับการสนับสนุนระบบเก่าและการผสานรวมกับเทคโนโลยีที่มีอยู่

## วิธีการ:
ใน Kotlin, คุณสามารถใช้ `javax.xml.parsers` ที่ติดตั้งมากับระบบสำหรับการแยกวิเคราะห์:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
เพื่อสร้างเอกสาร XML, คุณอาจใช้ `javax.xml.transform`:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
ตัวอย่างผลลัพธ์สำหรับการแปลงเอกสารเป็น String จะเป็นเพียงเนื้อหา XML ของคุณในรูปแบบสตริง

## ลงลึก
XML ได้รับการขนานนามว่าเป็นหัวมุมหลักของการพัฒนาเว็บและซอฟต์แวร์ตั้งแต่ยุค 90, ได้รับความนิยมเนื่องจากอ่านง่ายและมีโครงสร้างชั้นเชิง ถึงแม้จะมี JSON ที่ได้รับความนิยมสำหรับบริการเว็บเนื่องจากความเรียบง่ายและขนาดข้อความที่เล็กกว่า XML ยังคงเป็นที่นิยมในสภาพแวดล้อมขององค์กร, บริการเว็บแบบ SOAP และการกำหนดค่า (เช่น ไฟล์เลย์เอาท์ Android)

มีห้องสมุดและ API ต่างๆ นอกเหนือจากคุณสมบัติที่ติดตั้งมากับ Kotlin/Java สำหรับการจัดการ XML, เช่น Simple XML Serialization และ Jackson XML module. แต่ `javax.xml.parsers` และ `javax.xml.transform` มักจะเพียงพอต่อความต้องการโดยไม่ต้องเพิ่มการพึ่งพาภายนอก

เมื่อจัดการกับ XML ใน Kotlin, รายละเอียดการดำเนินงานหลัก ได้แก่ การจัดการกับการเข้ารหัสตัวละครอย่างเหมาะสมและการจัดการกับ XML entities เพื่อป้องกันการโจมตีด้วยการฉีด XML ระมัดระวังความซับซ้อนของเนมสเปซและการตรวจสอบสคีมาเมื่อแยกวิเคราะห์ XML เพื่อให้แน่ใจว่าข้อมูลมีความสมบูรณ์

## ดูเพิ่มเติม
- [เอกสาร Kotlin](https://kotlinlang.org/docs/reference/)
- [เอกสาร Java DOM](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML Module](https://github.com/FasterXML/jackson-dataformat-xml)
