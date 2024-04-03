---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:41.672552-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E1A\u0E21\u0E37\u0E2D\
  \u0E01\u0E31\u0E1A\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E04\
  \u0E32\u0E14\u0E04\u0E34\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\
  \u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u2026"
lastmod: '2024-03-17T21:57:56.652556-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E1A\u0E21\u0E37\u0E2D\
  \u0E01\u0E31\u0E1A\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E04\
  \u0E32\u0E14\u0E04\u0E34\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\
  \u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E25\u0E49\u0E21\u0E40\u0E2B\u0E25\
  \u0E27\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32\u0E15\u0E49\u0E2D\u0E07\
  \u0E23\u0E39\u0E49\u0E2A\u0E36\u0E01\u0E40\u0E04\u0E23\u0E35\u0E22\u0E14."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## การทำ:
เพื่อจับข้อผิดพลาดใน Fish ให้ใช้คำสั่ง `status` และเงื่อนไข สมมติ `ping` ล้มเหลว นี่คือวิธีตรวจจับ:

```fish
ping -c 1 example.com
if not status is-success
    echo "Something fishy happened with the ping."
end
```

ผลลัพธ์ตัวอย่างหาก `ping` ล้มเหลว:

```
Something fishy happened with the ping.
```

เพื่อจัดการกับรหัสข้อผิดพลาดเฉพาะ ให้ใช้ `status --is`:

```fish
false
if status --is 1
    echo "Caught an error with code 1."
end
```

ผลลัพธ์ตัวอย่าง:
```
Caught an error with code 1.
```

สำหรับการทำงานที่เชื่อถือได้มากว่า พิจารณาใช้ฟังก์ชัน:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping failed with status $status"
        return 1
    end
end

try_ping
```

## ลงลึก
การจัดการกับข้อผิดพลาดใน Fish ไม่ได้เป็นไปตามแบบฉบับ `try/catch` ที่คุณอาจคุ้นเคยจากภาษาการโปรแกรมระดับสูง แทนที่จะเป็นแบบนั้น คุณมีสถานะการออกที่ให้ไว้โดยคำสั่ง `status` เป็นอย่างตรงไปตรงมา

ตามประวัติศาสตร์ ในระบบที่คล้าย Unix สถานะการออก `0` หมายถึงความสำเร็จ ขณะที่ค่าที่ไม่เป็นศูนย์หมายถึงข้อผิดพลาด ซึ่งโดยทั่วไปแล้วสะท้อนถึงเหตุผลในการล้มเหลวที่แตกต่างกัน ข้อตกลงนี้ถูกใช้โดยส่วนใหญ่ของยูทิลิตี้บรรทัดคำสั่งและด้วยเหตุนี้ โดย Fish เองด้วย

ทางเลือกสำหรับการตรวจสอบสถานะใน Fish รวมถึงการจัดการสัญญาณผ่าน `trap` ในเชลล์อื่น ๆ แต่ Fish ชอบการตรวจสอบสถานะที่ชัดเจนมากขึ้น เนื่องจากมันง่ายและมีแนวโน้มที่จะเกิดผลข้างเคียงน้อยกว่า

ในทางการปฏิบัติ การจัดการกับข้อผิดพลาดใน Fish ยังคงเรียบง่ายแต่มีพลัง ใหญ่ชนิดเนื่องจากลักษณะที่ไม่บล็อคการทำงานและเน้นไวยากรณ์ที่ชัดเจน ตามที่แสดงในตัวอย่าง รหัสข้อผิดพลาดผสมผสานได้ดีกับฟังก์ชัน ช่วยให้การจัดการข้อผิดพลาดเป็นแบบโมดูลาร์และอ่านง่าย

## ดูเพิ่มเติม
- เอกสารการใช้งาน Fish เกี่ยวกับเงื่อนไข: https://fishshell.com/docs/current/language.html#conditionals
- คู่มือการใช้งาน Fish ในหัวข้อการจัดการข้อผิดพลาด: https://fishshell.com/docs/current/tutorial.html#error-handling
