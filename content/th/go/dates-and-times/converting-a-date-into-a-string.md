---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:42.566531-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Go, \u0E41\u0E1E\
  \u0E04\u0E40\u0E01\u0E08 `time` \u0E43\u0E2B\u0E49\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\
  \u0E27\u0E25\u0E32 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E08\u0E31\
  \u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A object `time.Time` \u0E40\u0E1B\u0E47\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `Format`\
  \ method \u0E02\u0E2D\u0E07\u0E1B\u0E23\u0E30\u0E40\u0E20\u0E17\u2026"
lastmod: '2024-04-05T21:54:01.031147-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Go, \u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08 `time` \u0E43\u0E2B\
  \u0E49\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 \u0E23\u0E27\u0E21\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A object\
  \ `time.Time` \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49 `Format` method \u0E02\u0E2D\u0E07\u0E1B\u0E23\u0E30\u0E40\
  \u0E20\u0E17 `time.Time` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E38\u0E14\u0E1B\
  \u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\u0E19\u0E35\u0E49 \u0E04\u0E38\u0E13\u0E15\u0E49\
  \u0E2D\u0E07\u0E23\u0E30\u0E1A\u0E38 layout string \u0E15\u0E32\u0E21\u0E40\u0E27\
  \u0E25\u0E32\u0E2D\u0E49\u0E32\u0E07\u0E2D\u0E34\u0E07 \"Mon Jan 2 15:04:05 MST\
  \ 2006\"."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
ใน Go, แพคเกจ `time` ให้ความสามารถในการทำงานกับวันที่และเวลา รวมถึงการจัดรูปแบบ object `time.Time` เป็นสตริง การใช้ `Format` method ของประเภท `time.Time` สำหรับจุดประสงค์นี้ คุณต้องระบุ layout string ตามเวลาอ้างอิง "Mon Jan 2 15:04:05 MST 2006"

### ตัวอย่าง:
```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // ได้วันที่และเวลาปัจจุบัน
	fmt.Println("Current Time:", currentTime)

	// จัดรูปแบบเวลาปัจจุบันเป็นรูปแบบ dd-mm-yyyy
	formattedDate := currentTime.Format("02-01-2006")
	fmt.Println("Formatted Date:", formattedDate)

	// จัดรูปแบบเวลาปัจจุบันเป็นรายละเอียดเพิ่มเติม
	detailedFormat := currentTime.Format("Mon, 02 Jan 2006 15:04:05 MST")
	fmt.Println("Detailed Formatted Date:", detailedFormat)
}
```

#### ตัวอย่างผลลัพธ์:
```
Current Time: 2023-04-12 11:45:20.312457 +0000 UTC
Formatted Date: 12-04-2023
Detailed Formatted Date: Wed, 12 Apr 2023 11:45:20 UTC
```

ผลลัพธ์จะแตกต่างกันขึ้นอยู่กับวันที่และเวลาปัจจุบันเมื่อรันโปรแกรม

## ศึกษาลึก:
ในบริบทของ Go, การจัดการวันที่และเวลา รวมถึงการจัดรูปแบบ จัดการโดยแพคเกจ `time` หลัก วิธีการจัดรูปแบบวันที่ใน Go, ที่ระบุโดย `Format` method โดยใช้ layout string รูปแบบเฉพาะ, มีความเฉพาะตัวเมื่อเปรียบเทียบกับภาษาโปรแกรมอื่นๆ ที่อาจใช้ตัวกำหนดรูปแบบง่ายๆ เช่น `%Y` สำหรับปี 4 หลัก วิธีการของ Go ต้องการให้นักพัฒนาจำเวลาอ้างอิงเฉพาะ: Mon Jan 2 15:04:05 MST 2006, เนื่องจากทำหน้าที่เป็นรูปแบบสำหรับการจัดรูปแบบหรือการแยกวิเคราะห์วันที่

วิธีการนี้, แม้แต่น้อยที่ไม่ตรงกับความคุ้นเคยของนักพัฒนากับฟังก์ชันการจัดรูปแบบคล้ายกับ strftime, ถูกออกแบบมาเพื่อความชัดเจนและหลีกเลี่ยงความสับสนของรูปแบบที่ขึ้นอยู่กับ locale เมื่อชินกับมันแล้ว หลายคนพบว่าวิธีการนี้ช่วยลดข้อผิดพลาดและปรับปรุงความสามารถในการอ่านโค้ด

นอกจากนี้, แนวทางของไลบรารีมาตรฐานของ Go หมายความว่าสำหรับกรณีการใช้งานทั่วไปส่วนใหญ่ ไลบรารีของบุคคลที่สามไม่จำเป็น สิ่งนี้ทำให้การจัดการความเสี่ยงลดลงและรับประกันพฤติกรรมที่สอดคล้องกันในโครงการต่างๆ อย่างไรก็ตาม เมื่อทำงานกับการแปลงโซนเวลาที่ซับซ้อนหรือการคำนวณวันที่ที่เกิดขึ้นซ้ำ นักพัฒนาอาจต้องดูแพคเกจเพิ่มเติมเช่น `github.com/rickar/cal` สำหรับการคำนวณวันหยุด หรือ `github.com/golang/time` สำหรับการจัดการเวลาที่มีรายละเอียดมากกว่าที่แพคเกจ `time` มาตรฐานเสนอ
