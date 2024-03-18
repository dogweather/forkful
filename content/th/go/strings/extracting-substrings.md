---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:11.292988-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E22\u0E48\u0E2D\u0E22\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\
  \u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E04\u0E37\u0E19\
  \u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\
  \u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\
  \u0E21\u0E15\u0E33\u0E41\u0E2B\u0E19\u0E48\u0E07\u0E02\u0E2D\u0E07\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.659258-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E22\u0E48\u0E2D\u0E22\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\
  \u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E04\u0E37\u0E19\
  \u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\
  \u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\
  \u0E21\u0E15\u0E33\u0E41\u0E2B\u0E19\u0E48\u0E07\u0E02\u0E2D\u0E07\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การสกัดสตริงย่อยเกี่ยวข้องกับการเรียกคืนส่วนที่เฉพาะเจาะจงของสตริงตามตำแหน่งของพวกมัน โปรแกรมเมอร์มักจะทำการดำเนินการนี้เพื่อประมวลผลหรือจัดการข้อมูลข้อความอย่างมีประสิทธิภาพ เช่น การวิเคราะห์ข้อมูลนำเข้า การตรวจสอบรูปแบบ หรือการเตรียมข้อมูลส่งออก

## วิธีการ:

ใน Go, ชนิด `string` เป็นสไลซ์แบบอ่านอย่างเดียวของไบต์ เพื่อสกัดสตริงย่อย หนึ่งหลักๆ จะใช้ syntax `slice` ควบคู่ไปกับฟังก์ชัน `len()` ที่ฝังไว้สำหรับการตรวจสอบความยาว และแพ็คเกจ `strings` สำหรับการดำเนินการที่ซับซ้อนมากขึ้น นี่คือวิธีที่คุณสามารถทำได้:

### การสไลซ์พื้นฐาน

```go
package main

import (
    "fmt"
)

func main() {
    str := "สวัสดี, โลก!"
    // สกัด "โลก"
    subStr := str[7:12]
    
    fmt.Println(subStr) // ผลลัพธ์: โลก
}
```

### การใช้แพ็คเกจ `strings`

สำหรับการสกัดสตริงย่อยที่ซับซ้อนมากขึ้น เช่น การสกัดสตริงหลังหรือก่อนสตริงย่อยที่เฉพาะเจาะจง คุณสามารถใช้แพ็คเกจ `strings` ได้

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=จอห์น โด"
    // สกัดสตริงย่อยหลัง "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // ผลลัพธ์: จอห์น โด
}
```

สิ่งสำคัญคือต้องทราบว่าสตริงใน Go ถูกเข้ารหัสด้วย UTF-8 และการสไลซ์ไบต์โดยตรงอาจไม่เสมอไปนำไปสู่สตริงที่ถูกต้องหากพวกมันรวมไบต์หลายไบต์ สำหรับการสนับสนุน Unicode ควรพิจารณาใช้ `range` หรือแพ็คเกจ `utf8`.

### การจัดการตัวอักษร Unicode

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "สวัสดี, 世界"
    // ค้นหาสตริงย่อยโดยพิจารณาตัวอักษร Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // ผลลัพธ์: 世界
}
```

## ศึกษาลึกลงไป

การสกัดสตริงย่อยใน Go เป็นเรื่องง่าย ด้วย syntax สไลซ์และห้องสมุดมาตรฐานที่ครอบคลุม ในอดีต ภาษาการเขียนโปรแกรมแต่ละสมัยมีฟังก์ชันหรือวิธีการโดยตรงมากขึ้นเพื่อจัดการการจัดการข้อความแบบนี้ อย่างไรก็ตาม วิธีการของ Go มีจุดเน้นที่ความปลอดภัยและประสิทธิภาพ โดยเฉพาะกับสตริงที่ไม่เปลี่ยนแปลงและการจัดการตัวอักษร Unicode อย่างชัดเจนผ่าน runes

ในขณะที่การสไลซ์อย่างตรงไปตรงมาได้รับประโยชน์จากประสิทธิภาพ แต่ก็รับมรดกของความซับซ้อนในการจัดการตัวละคร UTF-8 โดยตรงการแนะนำประเภท `rune` ช่วยให้โปรแกรม Go สามารถจัดการข้อความ Unicode ได้อย่างปลอดภัย ทำให้เป็นทางเลือกที่ทรงพลังสำหรับแอปพลิเคชันสากล

นอกจากนี้ โปรแกรมเมอร์ที่มาจากภาษาอื่นอาจจะพลาดฟังก์ชันการจัดการสตริงระดับสูงที่ติดตั้งไว้อย่างตรงไปตรงมา อย่างไรก็ตาม แพ็คเกจ `strings` และ `bytes` ในห้องสมุดมาตรฐานของ Go นำเสนอชุดฟังก์ชันที่หลากหลายซึ่ง แม้ว่าจะต้องใช้โค้ดเพิ่มเติม แต่ก็ให้ตัวเลือกที่ทรงพลังสำหรับการประมวลผลสตริง รวมถึงการสกัดสตริงย่อย

โดยสรุป การเลือกออกแบบของ Go รอบๆ การจัดการสตริงสะท้อนถึงเป้าหมายสำหรับความเรียบง่าย ประสิทธิภาพ และความปลอดภัยในการจัดการข้อมูลข้อความสมัยใหม่ที่ล้ำหน้าในขณะที่อาจจำเป็นต้องปรับตัวเล็กน้อย Go นำเสนอเครื่องมือที่มีประสิทธิภาพและมีประสิทธิผลสำหรับการจัดการการสกัดสตริงย่อยและอื่นๆ