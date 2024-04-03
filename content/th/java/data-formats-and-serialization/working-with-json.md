---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:16.407523-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E44\u0E1B\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E01\u0E31\u0E1A JSON \u0E43\u0E19 Java \u0E01\
  \u0E31\u0E19\u0E40\u0E16\u0E2D\u0E30 \u0E02\u0E31\u0E49\u0E19\u0E41\u0E23\u0E01\
  , \u0E04\u0E38\u0E13\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E49\u0E2D\u0E07\
  \u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E01\u0E32\u0E23\u0E1B\u0E23\
  \u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25 JSON \u0E40\u0E0A\u0E48\u0E19 `Jackson` \u0E2B\
  \u0E23\u0E37\u0E2D `Google Gson` \u0E17\u0E35\u0E48\u0E19\u0E35\u0E48\u0E40\u0E23\
  \u0E32\u0E08\u0E30\u0E43\u0E0A\u0E49 `Jackson`\u2026"
lastmod: '2024-03-17T21:57:56.107380-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E1B\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E01\
  \u0E31\u0E1A JSON \u0E43\u0E19 Java \u0E01\u0E31\u0E19\u0E40\u0E16\u0E2D\u0E30\n\
  \n\u0E02\u0E31\u0E49\u0E19\u0E41\u0E23\u0E01, \u0E04\u0E38\u0E13\u0E08\u0E33\u0E40\
  \u0E1B\u0E47\u0E19\u0E15\u0E49\u0E2D\u0E07\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25 JSON\
  \ \u0E40\u0E0A\u0E48\u0E19 `Jackson` \u0E2B\u0E23\u0E37\u0E2D `Google Gson` \u0E17\
  \u0E35\u0E48\u0E19\u0E35\u0E48\u0E40\u0E23\u0E32\u0E08\u0E30\u0E43\u0E0A\u0E49 `Jackson`\
  \ \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E04\
  \u0E27\u0E32\u0E21\u0E02\u0E36\u0E49\u0E19\u0E2D\u0E22\u0E39\u0E48\u0E19\u0E35\u0E49\
  \u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C `pom.xml` \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  ."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีทำ:
ไปเขียนโค้ดกับ JSON ใน Java กันเถอะ

ขั้นแรก, คุณจำเป็นต้องมีไลบรารีการประมวลผล JSON เช่น `Jackson` หรือ `Google Gson` ที่นี่เราจะใช้ `Jackson` ดังนั้นเพิ่มความขึ้นอยู่นี้ในไฟล์ `pom.xml` ของคุณ:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

ตอนนี้, มาซีเรียลไลซ์ (เขียน) วัตถุ Java ง่ายๆ เป็น JSON กัน:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

ผลลัพธ์ควรจะเป็น:

```json
{"name":"Alex","age":30}
```

ตอนนี้, เพื่อถอดรหัสไฟลที่เป็น JSON กลับเป็นวัตถุ Java :

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " years old.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

ผลลัพธ์จะเป็น:

```
Alex is 30 years old.
```

## ลงลึก
ความง่ายและความมีประสิทธิภาพของ JSON ทำให้มันกลายเป็นมาตรฐานอย่างไม่เป็นทางการสำหรับการแลกเปลี่ยนข้อมูลบนเว็บ โค่นล้ม XML ออกจากบัลลังก์ ถูกแนะนำในช่วงต้นปี 2000s, JSON ถูกอนุมานมาจาก JavaScript แต่ตอนนี้ได้รับการสนับสนุนในภาษาส่วนใหญ่

ทางเลือกสำหรับ JSON รวมถึง XML ซึ่งมีรายละเอียดเพิ่มเติม, และรูปแบบไบนารีเช่น Protocol Buffers หรือ MessagePack ซึ่งไม่ง่ายต่อการอ่านโดยมนุษย์ แต่มีความมีประสิทธิภาพมากขึ้นในเรื่องขนาดและความเร็ว แต่ละอย่างมีกรณีการใช้งานของตัวเอง; การเลือกขึ้นอยู่กับความต้องการข้อมูลและบริบทเฉพาะของคุณ

ใน Java, นอกเหนือจาก `Jackson` และ `Gson`, เรามี `JsonB` และ `org.json` เป็นไลบรารีอื่นๆ เพื่อจัดการกับ JSON Jackson นำเสนอการประมวลผลแบบสตรีมและเป็นที่รู้จักกันดีเรื่องความเร็ว, ในขณะที่ Gson ได้รับการยกย่องเพราะความง่ายในการใช้งาน JsonB เป็นส่วนหนึ่งของ Jakarta EE, นำเสนอแนวทางที่มาตรฐานมากขึ้น

เมื่อทำงานกับ JSON, จำไว้ว่าต้องจัดการกับข้อยกเว้นของคุณอย่างเหมาะสม - โค้ดของคุณควรจะมีความแข็งแรงต่อข้อมูลที่ไม่ดี ยัง, พิจารณาถึงผลการดำเนินงานด้านความปลอดภัยของการผูกข้อมูลอัตโนมัติ – ตรวจสอบข้อมูลขาเข้าของคุณเสมอ!

## ดูเพิ่มเติม
- [Jackson Project](https://github.com/FasterXML/jackson)
- [Gson Project](https://github.com/google/gson)
- [JSON Specification](https://www.json.org/json-en.html)
- [JsonB Specification](https://jakarta.ee/specifications/jsonb/)
