---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:26.421879-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\
  \u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing)\
  \ \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 (querying) \u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E01\u0E2A\u0E32\
  \u0E23 XML \u0E14\u0E49\u0E27\u0E22 Java \u0E19\u0E31\u0E01\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E15\u0E48\u0E32\u0E07\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.110789-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E1B\
  \u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E41\u0E22\
  \u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing) \u0E01\u0E32\
  \u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 (querying) \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 XML \u0E14\
  \u0E49\u0E27\u0E22 Java \u0E19\u0E31\u0E01\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E15\u0E48\u0E32\u0E07\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\
  \u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การทำงานกับ XML ประกอบด้วยการแยกวิเคราะห์ (parsing) การค้นหา (querying) และการจัดการเอกสาร XML ด้วย Java นักโปรแกรมต่างทำเช่นนี้เพื่อการแลกเปลี่ยนข้อมูล, การจัดการคอนฟิก, และเนื่องจากหลายระบบเดิมและ API ต่างสื่อสารกันผ่าน XML

## วิธีการ:
Java มี API เช่น DOM (Document Object Model), SAX (Simple API for XML), และ StAX (Streaming API for XML) สำหรับทำงานกับ XML นี่คือตัวอย่างของ DOM ในการแยกวิเคราะห์ไฟล์ XML:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Name: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Age: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

สมมติว่า `data.xml` มีลักษณะดังนี้:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

ผลลัพธ์จะเป็น:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## ดำดิ่งลงไป
XML เป็นที่นิยมใช้ตั้งแต่ปลายยุค '90, มอบวิธีการที่มีโครงสร้างและยืดหยุ่นในการแลกเปลี่ยนข้อมูลข้ามระบบที่แตกต่างกัน แม้ว่า JSON จะได้รับความนิยมมากขึ้นสำหรับ web APIs ใหม่ๆ เนื่องจากไวยากรณ์ที่ง่ายกว่าและการรวมตัวกับ JavaScript อย่างแน่นหนา แต่ XML ยังคงถูกใช้อย่างแพร่หลายในสภาพแวดล้อมขององค์กร, บริการเว็บที่ใช้ SOAP, และมาตรฐานเอกสารเช่น Office Open XML สำหรับ Microsoft Office

เมื่อพูดถึงการแยกวิเคราะห์ XML ใน Java, DOM API เหมาะสำหรับเอกสารขนาดเล็ก: มันเป็นแบบต้นไม้และอนุญาตให้เข้าถึงโครงสร้าง XML ในหน่วยความจำได้อย่างเต็มที่ อย่างไรก็ตามสำหรับไฟล์ขนาดใหญ่ มันอาจใช้หน่วยความจำมาก SAX และ StAX เป็นมิตรกับหน่วยความจำมากขึ้น เนื่องจากเป็นแบบขับเคลื่อนโดยเหตุการณ์และแบบสตรีมตามลำดับ แต่อาจไม่สะดวกสำหรับการนำทางโครงสร้าง XML

สำหรับการสร้างหรือการแก้ไข XML, Java ยังมีแพคเกจ javax.xml.transform และ javax.xml.bind (JAXB) JAXB เป็นส่วนหนึ่งของ Java SE จนถึงเวอร์ชัน 10, หลังจากนั้น, มันกลายเป็นไลบรารีแยกต่างหากเนื่องจากการลบโมดูล Java EE ออกจาก Java SE มันเป็นวิธีการที่ขับเคลื่อนโดย annotation ในการ serialize อ็อบเจกต์ Java เป็น XML และทำกลับกัน

## ดูเพิ่มเติม
ตรวจสอบแหล่งข้อมูลเหล่านี้เพื่อเรียนรู้เพิ่มเติมเกี่ยวกับการทำงานกับ XML ใน Java:
- [Java API สำหรับการประมวลผล XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [สถาปัตยกรรม Java สำหรับการผูก XML (JAXB)](https://javaee.github.io/jaxb-v2/)
- [คู่มือ Oracle เกี่ยวกับ XML ใน Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [เทคโนโลยี W3C XML](https://www.w3.org/standards/xml/)
- [Stack Overflow: คำถามที่มีแท็ก 'java' และ 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
