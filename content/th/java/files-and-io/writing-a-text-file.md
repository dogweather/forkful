---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:53.690103-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: #."
lastmod: '2024-03-17T21:57:56.100750-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีทำ:


### การใช้ `java.nio.file` (Standard Library)
Java's New IO (NIO) package (`java.nio.file`) ให้วิธีการที่หลากหลายมากขึ้นสำหรับการจัดการกับไฟล์ นี่คือวิธีง่ายๆ ในการเขียนลงไฟล์โดยใช้ `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("บรรทัดที่ 1", "บรรทัดที่ 2", "บรรทัดที่ 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("เขียนไฟล์สำเร็จแล้ว!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

ผลลัพธ์:

```
เขียนไฟล์สำเร็จแล้ว!
```

### การใช้ `java.io` (Standard Library)
สำหรับวิธีการที่ดั้งเดิมยิ่งขึ้น `java.io.FileWriter` เป็นตัวเลือกที่ดีสำหรับการเขียนไฟล์ข้อความอย่างง่าย:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("สวัสดี, โลก!\n");
            writer.append("นี่คือบรรทัดอื่น.");
            System.out.println("เขียนไฟล์สำเร็จแล้ว!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

ผลลัพธ์:

```
เขียนไฟล์สำเร็จแล้ว!
```

### การใช้ Apache Commons IO
ห้องสมุด Apache Commons IO ทำให้การดำเนินการหลายอย่างเรียบง่ายขึ้น รวมถึงการเขียนไฟล์ นี่คือวิธีเขียนข้อความลงไฟล์โดยใช้ `FileUtils.writeStringToFile()`:

ก่อนอื่น, เพิ่มการอ้างอิง (dependency) ลงในโปรเจกต์ของคุณ หากใช้ Maven, ใส่:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- ตรวจสอบเวอร์ชั่นล่าสุด -->
</dependency>
```

จากนั้น, ใช้โค้ดดังต่อไปนี้เพื่อเขียนข้อความลงไฟล์:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "นี่คือข้อความที่เขียนโดยใช้ Commons IO.", "UTF-8");
            System.out.println("เขียนไฟล์สำเร็จแล้ว!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

ผลลัพธ์:

```
เขียนไฟล์สำเร็จแล้ว!
```
