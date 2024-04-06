---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:06.423066-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Java \u0E21\
  \u0E35\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35 directory\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E42\u0E14\u0E22\u0E2B\u0E25\u0E31\
  \u0E01\u0E46 \u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `java.nio.file.Files` \u0E41\
  \u0E25\u0E30 `java.io.File` **\u0E43\u0E0A\u0E49 `java.nio.file.Files`**:\u2026"
lastmod: '2024-04-05T21:54:01.707121-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Java \u0E21\u0E35\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\
  \u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\
  \u0E48\u0E32\u0E21\u0E35 directory \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E42\
  \u0E14\u0E22\u0E2B\u0E25\u0E31\u0E01\u0E46 \u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A\
  \ `java.nio.file.Files` \u0E41\u0E25\u0E30 `java.io.File` **\u0E43\u0E0A\u0E49 `java.nio.file.Files`**."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
ใน Java มีหลายวิธีในการตรวจสอบว่ามี directory หรือไม่ โดยหลักๆ ใช้คลาส `java.nio.file.Files` และ `java.io.File`

**ใช้ `java.nio.file.Files`**:

นี่คือวิธีที่แนะนำในเวอร์ชั่น Java ล่าสุด

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // ระบุ path ของ directory ที่นี่
        String directoryPath = "path/to/directory";

        // ตรวจสอบว่ามี directory หรือไม่
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("มี directory นี้อยู่.");
        } else {
            System.out.println("ไม่มี directory นี้อยู่.");
        }
    }
}
```
**ผลลัพธ์ตัวอย่าง**:
```
มี directory นี้อยู่.
```
หรือ 
```
ไม่มี directory นี้อยู่.
```

**ใช้ `java.io.File`**:

แม้ว่าจะแนะนำ `java.nio.file.Files` แต่คลาสเก่า `java.io.File` ก็สามารถใช้ได้

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // ระบุ path ของ directory ที่นี่
        String directoryPath = "path/to/directory";

        // สร้างออบเจค File
        File directory = new File(directoryPath);

        // ตรวจสอบว่ามี directory หรือไม่
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("มี directory นี้อยู่.");
        } else {
            System.out.println("ไม่มี directory นี้อยู่.");
        }
    }
}
```
**ผลลัพธ์ตัวอย่าง**:
```
มี directory นี้อยู่.
```
หรือ
```
ไม่มี directory นี้อยู่.
```

**ใช้ไลบรารีของบุคคลที่สาม**:

แม้ว่าไลบรารีมาตรฐานของ Java มักเพียงพอสำหรับงานนี้ ไลบรารีของบุคคลที่สามเช่น Apache Commons IO นำเสนอเครื่องมือการจัดการไฟล์เพิ่มเติมที่อาจมีประโยชน์ในแอปพลิเคชันที่ซับซ้อนยิ่งขึ้น

**Apache Commons IO**:

ก่อนอื่น เพิ่ม dependency ของ Apache Commons IO ในโปรเจกต์ของคุณ จากนั้นคุณสามารถใช้คุณสมบัติของมันในการตรวจสอบการมีอยู่ของ directory

```java
// สมมติว่า Apache Commons IO ถูกเพิ่มในโปรเจกต์

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // ระบุ path ของ directory ที่นี่
        String directoryPath = "path/to/directory";

        // ใช้ FileUtils เพื่อตรวจสอบ
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("มี directory นี้อยู่.");
        } else {
            System.out.println("ไม่มี directory นี้อยู่.");
        }
    }
}
```

**หมายเหตุ**: `FileUtils.directoryContains` ตรวจสอบว่า directory มีไฟล์เฉพาะหรือไม่ แต่โดยการส่ง `null` เป็นอาร์กิวเมนต์ที่สอง คุณสามารถใช้มันเพื่อตรวจสอบการมีอยู่ของ directory นั่น ระวัง เนื่องจากนี่อาจไม่ใช่วิธีการใช้งานที่ง่ายที่สุดหรือตั้งใจไว้

**ผลลัพธ์ตัวอย่าง**:
```
มี directory นี้อยู่.
```
หรือ
```
ไม่มี directory นี้อยู่.
```
