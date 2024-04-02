---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:59.494383-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07 \u0E20\u0E32\u0E29\
  \u0E32\u0E17\u0E35\u0E48\u0E40\u0E2B\u0E47\u0E19\u0E44\u0E14\u0E49\u0E0A\u0E31\u0E14\
  \u0E40\u0E08\u0E19\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\
  \u0E22\u0E02\u0E2D\u0E07\u0E17\u0E2D\u0E21 \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\
  \u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.109859-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07 \u0E20\u0E32\u0E29\u0E32\u0E17\
  \u0E35\u0E48\u0E40\u0E2B\u0E47\u0E19\u0E44\u0E14\u0E49\u0E0A\u0E31\u0E14\u0E40\u0E08\
  \u0E19\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E02\
  \u0E2D\u0E07\u0E17\u0E2D\u0E21 \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\
  \u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## คืออะไร & ทำไม?
TOML ย่อมาจาก Tom's Obvious, Minimal Language หมายถึง ภาษาที่เห็นได้ชัดเจนและเรียบง่ายของทอม มันเป็นรูปแบบการทำให้ข้อมูลมีโครงสร้างสำหรับไฟล์การตั้งค่า โปรแกรมเมอร์ใช้มันเพราะมันอ่านง่าย เขียนง่าย และจับคู่กับตารางแฮชได้ดี

## วิธีการ:
คุณจะต้องมีไลบรารีที่ใช้ในการแยกวิเคราะห์ TOML ฉันแนะนำ `toml4j` ให้เพิ่มมันเข้าไปในโปรเจกต์ของคุณดังนี้:

```java
// เพิ่มสิ่งนี้เข้าไปใน build.gradle ของคุณ
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

นี่คือวิธีที่คุณแยกวิเคราะห์ไฟล์ TOML:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("IP ของเซิร์ฟเวอร์: " + ip);
        System.out.println("พอร์ตของเซิร์ฟเวอร์: " + port);
    }
}
```

ตัวอย่างผลลัพธ์:

```
IP ของเซิร์ฟเวอร์: 192.168.1.1
พอร์ตของเซิร์ฟเวอร์: 80
```

## ลงลึกลงไป
TOML ถูกพัฒนาโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub มีจุดมุ่งหมายเพื่อให้ง่ายกว่า XML และมีการระบุเพิ่มเติมกว่า YAML รุ่นล่าสุด 1.0.0 ซึ่งเปิดตัวในปี 2021 นำเสนอชุดคุณสมบัติที่คงที่

ตัวเลือกเช่น JSON หรือ YAML ก็ได้รับความนิยมเช่นกัน JSON เหมาะสำหรับการแลกเปลี่ยนข้อมูล YAML มีความอ่านง่ายมากขึ้นสำหรับการตั้งค่าที่ซับซ้อน TOML เป็นที่นิยมเพราะความตรงไปตรงมาและการใช้งานในชุมชนของ Rust

ในด้านการใช้งาน TOML กับ Java ควรจำไว้ว่าการเลือกพาเซอร์นั้นสำคัญ นอกเหนือจาก `toml4j` บางคนอาจเลือกใช้ `jackson-dataformat-toml` แต่ละตัวจะมีความแตกต่างเช่น การจัดการข้อผิดพลาดหรือประสิทธิภาพในการแยกวิเคราะห์ ดังนั้น ให้เลือกตามความต้องการของโปรเจกต์ของคุณ

## ดูเพิ่มเติม
- ข้อกำหนด TOML: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
