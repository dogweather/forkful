---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:02.080071-06:00
description: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34 (regex)\
  \ \u0E43\u0E19 Java \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23 \u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.070151-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34 (regex)\
  \ \u0E43\u0E19 Java \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23 \u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## อะไรและทำไม?

นิพจน์ปรกติ (regex) ใน Java ช่วยให้คุณกำหนดรูปแบบเฉพาะเพื่อค้นหา จัดการ หรือตรวจสอบสตริงในโค้ดของคุณ นักพัฒนาโปรแกรมใช้พวกมันสำหรับงานเช่น การแยกวิเคราะห์ไฟล์ log การตรวจสอบข้อมูลที่ผู้ใช้ป้อน หรือการค้นหารูปแบบเฉพาะภายในข้อความ ทำให้การประมวลผลสตริงเป็นไปได้โดยไม่ต้องใช้ความพยายามอย่างมาก

## วิธีการ:

การสนับสนุนเรื่อง regex ใน Java หลักๆ อยู่ที่คลาส `Pattern` และ `Matcher` ในแพ็คเกจ `java.util.regex` นี่คือตัวอย่างง่ายๆ ในการค้นหาและพิมพ์คำทั้งหมดที่พบในสตริง โดยไม่คำนึงถึงตัวเล็กตัวใหญ่:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("พบ '" + matcher.group() + "' ที่ตำแหน่ง " + matcher.start());
        }
    }
}
```

ผลลัพธ์:
```
พบ 'parsing' ที่ตำแหน่ง 16
พบ 'Parsing' ที่ตำแหน่ง 31
```

สำหรับงานเช่นการแยกสตริง คุณสามารถใช้เมธอด `split()` ของคลาส `String` ร่วมกับ regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

ผลลัพธ์:
```
Java
Python
Ruby
JavaScript
```

เมื่อทำงานกับ regex ใน Java อาจมีกรณีที่ไลบรารีภายนอกสามารถทำให้งานซับซ้อนง่ายขึ้นได้ หนึ่งในไลบรารีบุคคลที่สามที่นิยมสำหรับทำงานกับ regex ใน Java คือ `Apache Commons Lang` มันเสนอยูทิลิตี้เช่น `StringUtils` ที่ทำให้งานบางอย่างเกี่ยวกับ regex ง่ายขึ้น นี่คือวิธีใช้งานเพื่อนับจำนวนของการตรงกันของ substring:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' ปรากฏ " + count + " ครั้ง.");
    }
}
```

ในการใช้ Apache Commons Lang คุณจำเป็นต้องรวมมันเข้ากับโปรเจกต์ของคุณ หากคุณใช้ Maven เพิ่ม dependency นี้ในไฟล์ `pom.xml` ของคุณ:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- ตรวจสอบเวอร์ชันล่าสุด -->
</dependency>
```

ผลลัพธ์:
```
'processing' ปรากฏ 2 ครั้ง.
```
