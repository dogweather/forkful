---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:03.859616-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Go \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E2B\
  \u0E49\u0E01\u0E25\u0E32\u0E22\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E21\
  \u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 (\u0E40\u0E0A\u0E48\u0E19 `time.Time`).\u2026"
lastmod: '2024-03-17T21:57:55.682652-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Go \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E2B\
  \u0E49\u0E01\u0E25\u0E32\u0E22\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E21\
  \u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 (\u0E40\u0E0A\u0E48\u0E19 `time.Time`).\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การแยกวิเคราะห์วันที่จากสตริงใน Go หมายถึงการแปลงวันที่ที่เป็นข้อความให้กลายเป็นรูปแบบที่ใช้งานได้มากขึ้น (เช่น `time.Time`). โปรแกรมเมอร์ทำการนี้เพื่อจัดการข้อมูลวันที่และเวลาได้อย่างแม่นยำยิ่งขึ้นในแอปพลิเคชั่น โดยเฉพาะเมื่อต้องทำงานกับข้อมูลจากผู้ใช้ หรือจาก API หรือระบบจัดเก็บข้อมูลที่วันที่มักจะเป็นข้อความ

## วิธีการ:

Go ให้การสนับสนุนที่แข็งแกร่งสำหรับการแยกวิเคราะห์วันที่และเวลาผ่านแพ็กเกจ `time` กุญแจสำคัญคือการเข้าใจรูปแบบวันที่อ้างอิงของ Go: `Mon Jan 2 15:04:05 MST 2006` ซึ่งคุณใช้มันเพื่อบอก Go ว่าจะตีความสตริงที่รับเข้ามาอย่างไร นี่คือตัวอย่างเริ่มต้นเพื่อให้คุณเริ่มต้น:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// ตัวอย่างวันที่สตริง
	dateStr := "2023-04-12 14:45:00"
	
	// กำหนดรูปแบบ/รูปแบบของวันที่สตริงที่รับเข้า
	// รูปแบบนี้บอก Go ว่าคาดหวังปี ตามด้วยเดือน
	// แล้วคือวัน ชั่วโมง นาที และวินาที
	layout := "2006-01-02 15:04:05"
	
	// แยกวิเคราะห์วันที่สตริงตามรูปแบบ
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}
	
	// แสดงผลวันที่ที่แยกวิเคราะห์ได้
	fmt.Println("Parsed Date:", parsedDate)
}
```

เมื่อคุณรันโค้ดนี้ คุณจะได้รับ:

```
Parsed Date: 2023-04-12 14:45:00 +0000 UTC
```

สังเกตว่าสตริง `layout` ใช้ค่าของวันที่อ้างอิงเพื่อระบุรูปแบบของสตริงที่รับเข้า ปรับแต่ง `layout` เพื่อให้ตรงกับรูปแบบของวันที่ที่คุณรับเข้ามา

## ลงลึก

การออกแบบการแยกวิเคราะห์วันที่และเวลาของ Go มีเอกลักษณ์ โดยใช้วันที่อ้างอิงเฉพาะ (`Mon Jan 2 15:04:05 MST 2006`) วิธีนี้ถูกเลือกมาเพื่อความสามารถในการอ่านออกเขียนได้ง่ายและความสะดวกในการใช้งาน มากกว่าการใช้ตัวกำหนดรูปแบบที่แพร่หลายกว่า (เช่น `YYYY` สำหรับปี)

ในขณะที่วิธีนี้อาจดูผิดปกติสำหรับโปรแกรมเมอร์ที่คุ้นเคยกับภาษาอื่น หลายคนพบว่ามันมีความใช้งานง่ายขึ้นหลังจากช่วงเวลาปรับตัวสั้นๆ สำหรับแอปพลิเคชั่นที่ต้องการการจัดการวันที่ที่ซับซ้อนขึ้นหรือรูปแบบที่ไม่ได้รับการสนับสนุนโดยตรงจากแพ็กเกจ `time` ของ Go ไลบรารีของบุคคลที่สามเช่น `github.com/jinzhu/now` สามารถเสนอฟังก์ชันการทำงานเพิ่มเติม อย่างไรก็ตาม สำหรับแอปพลิเคชั่นมาตรฐานส่วนใหญ่ ความสามารถภายในของ Go นั้นแข็งแกร่ง มีประสิทธิภาพ และสอดคล้องกับปรัชญาของ Go ในเรื่องความเรียบง่ายและชัดเจน