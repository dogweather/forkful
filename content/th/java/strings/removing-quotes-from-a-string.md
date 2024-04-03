---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:35.394291-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E14\
  \u0E36\u0E07\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E2D\
  \u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\
  \u0E31\u0E49\u0E19\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32 \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\
  \u0E07\u0E32\u0E19 `replace()` method \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27 \u0E41\u0E25\u0E30\u0E43\u0E0A\u0E49 regex \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\u0E22\u0E32\u0E01\
  \u0E02\u0E36\u0E49\u0E19."
lastmod: '2024-03-17T21:57:56.068251-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E14\u0E36\u0E07\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\
  \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\u0E31\u0E49\u0E19\u0E2D\u0E2D\u0E01\u0E08\u0E32\
  \u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32\
  \ \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 `replace()` method \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27 \u0E41\u0E25\u0E30\
  \u0E43\u0E0A\u0E49 regex \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E17\
  \u0E35\u0E48\u0E22\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## วิธีการ:
ลองดึงเครื่องหมายอัญประกาศเหล่านั้นออกจากข้อความของเรา การใช้งาน `replace()` method สำหรับการแก้ไขอย่างรวดเร็ว และใช้ regex สำหรับงานที่ยากขึ้น

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // ตอนนี้กับ regex สำหรับผู้ที่ชื่นชอบรูปแบบ
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## ศึกษาลึกลงไป
ก่อนหน้านี้ เครื่องหมายอัญประกาศในสตริงไม่ได้ถือเป็นปัญหามากนัก—ระบบมีความเรียบง่ายและข้อมูลไม่ได้ซับซ้อน ด้วยการมาของรูปแบบข้อมูลที่ซับซ้อน (JSON, XML) และความต้องการในการแลกเปลี่ยนข้อมูล การจัดการเครื่องหมายอัญประกาศจึงกลายเป็นสิ่งสำคัญ ถ้าพูดถึงทางเลือก แน่นอนคุณสามารถเขียน parser, วนลูปทุกตัวอักษร, และสร้างสตริงใหม่ (อาจเป็นสิ่งน่าสนใจในวันที่ฝนตก) ยังมีไลบรารีของบุคคลที่สามที่สามารถจัดการเรื่องนี้ได้ด้วยความประณีตมากขึ้น ให้ตัวเลือกในการหลบหนีตัวละครแทนที่จะลบออก หรือจัดการกับประเภทของเครื่องหมายอัญประกาศตาม locale ในด้านการใช้งาน โปรดจำไว้ว่าการลบเครื่องหมายอัญประกาศโดยไม่มีบริบทอาจเปลี่ยนความหมายหรือโครงสร้างของข้อมูล—เสมอพิจารณา "ทำไม" ก่อน "วิธีการ"

## ดูเพิ่มเติม
- สำหรับการศึกษารูปแบบลึกลงไป ตรวจสอบเอกสาร Java อย่างเป็นทางการ: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- จำเป็นต้องหลบหนีเครื่องหมายอัญประกาศแทนการลบทิ้งหรือไม่? Stack Overflow สามารถช่วยคุณได้: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- การประมวลผล JSON ใน Java? คุณอาจพบเครื่องหมายอัญประกาศบ่อยครั้ง นี่คือจุดเริ่มต้น: https://www.oracle.com/technical-resources/articles/java/json.html
