---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:50.360832-06:00
description: ''
lastmod: '2024-04-05T22:00:12.737869-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:


### การใช้งานแพคเกจ `java.time` (แนะนำใน Java 8 ขึ้นไป):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // ผลลัพธ์: 2023-04-30
    }
}
```

### การใช้ `SimpleDateFormat` (วิธีการเดิม):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // รูปแบบผลลัพธ์ขึ้นอยู่กับรูปแบบมาตรฐานของระบบของคุณ
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### การใช้ไลบรารีของบุคคลที่สาม (เช่น, Joda-Time):
Joda-Time เป็นไลบรารีของบุคคลที่สามที่สำคัญ แต่ตอนนี้อยู่ในโหมดการบำรุงรักษาเนื่องจากการเปิดตัวแพคเกจ `java.time` ใน Java 8 อย่างไรก็ตาม, สำหรับผู้ที่ใช้ Java เวอร์ชันก่อน 8, Joda-Time เป็นตัวเลือกที่ดี
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // ผลลัพธ์: 2023-04-30
    }
}
```
โปรดทราบว่าเมื่อทำงานกับวันที่, ควรตระหนักถึงการตั้งค่าเขตเวลาหากคุณกำลังวิเคราะห์หรือจัดรูปแบบวันที่และเวลามากกว่าแค่วันที่
