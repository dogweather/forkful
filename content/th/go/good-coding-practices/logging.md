---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:39.418335-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Go, logging\
  \ \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19 `log` \u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08\u0E19\u0E35\u0E49\u0E21\
  \u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\
  \u0E01\u0E32\u0E23 logging \u0E17\u0E35\u0E48\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\
  \u0E48\u0E32\u0E22 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 standard output \u0E2B\u0E23\u0E37\u0E2D\
  \u0E44\u0E1F\u0E25\u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.679374-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Go, logging \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\
  \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E04\u0E40\u0E01\
  \u0E08\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 `log` \u0E41\u0E1E\u0E04\u0E40\u0E01\
  \u0E08\u0E19\u0E35\u0E49\u0E21\u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23 logging \u0E17\u0E35\u0E48\u0E40\
  \u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22 \u0E40\u0E0A\u0E48\u0E19 \u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 standard\
  \ output \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E1F\u0E25\u0E4C \u0E25\u0E2D\u0E07\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E01\u0E32\
  \u0E23 logging \u0E44\u0E1B\u0E22\u0E31\u0E07 standard output."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีการ:
ใน Go, logging สามารถทำได้โดยใช้แพคเกจมาตรฐาน `log` แพคเกจนี้มอบความสามารถในการ logging ที่เรียบง่าย เช่น การเขียนไปยัง standard output หรือไฟล์ ลองเริ่มด้วยตัวอย่างพื้นฐานของการ logging ไปยัง standard output:

```go
package main

import (
	"log"
)

func main() {
	log.Println("This is a basic log entry.")
}
```

ผลลัพธ์:
```
2009/11/10 23:00:00 This is a basic log entry.
```

Timestamp ที่ต้นของ log entry จะถูกเพิ่มโดยอัตโนมัติโดยแพคเกจ `log` ถัดไป ลองดูการ log ไปยังไฟล์แทนการเขียนออกมาที่ standard output:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("This log entry goes to a file.")
}
```

ตอนนี้ ลองทำตัวอย่างเคสที่ซับซ้อนกว่า: การปรับแต่งรูปแบบของการ logging Go ช่วยให้คุณสร้าง custom logger ด้วย `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "CUSTOM LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("This is a custom log message.")
}
```

ผลลัพธ์:
```
CUSTOM LOG: 2009/11/10 23:00:00 main.go:11: This is a custom log message.
```

ตัวอย่างนี้จะเติมข้อความ "CUSTOM LOG: " บนทุกข้อความ log และรวมวันที่ เวลา และตำแหน่งไฟล์ต้นฉบับ

## การดำดิ่งลึก
แพคเกจ `log` ของไลบรารีมาตรฐานของ Go มีความเรียบง่ายและเพียงพอสำหรับแอปพลิเคชันมากมาย แต่ก็ขาดคุณสมบัติที่ซับซ้อนยิ่งขึ้นที่พบในไลบรารีคอลเลกชันการ logging ของบุคคลที่สาม เช่น structured logging, log rotation และระดับการ logging แพคเกจเช่น `zap` และ `logrus` นำเสนอคุณสมบัติขั้นสูงเหล่านี้และเป็นที่ยอมรับในชุมชน Go สำหรับประสิทธิภาพและความยืดหยุ่นของพวกเขา

ตัวอย่างเช่น structured logging ช่วยให้คุณบันทึกข้อมูลในรูปแบบที่มีโครงสร้าง (เช่น JSON) ซึ่งมีประโยชน์โดยเฉพาะสำหรับแอปพลิเคชันที่อยู่บนคลาวด์สมัยใหม่ ที่อาจมีการวิเคราะห์โดยเครื่องมือหรือบริการต่างๆ `zap` โดยเฉพาะเป็นที่รู้จักสำหรับประสิทธิภาพที่สูงและ overhead ที่ต่ำของการจัดสรร ทำให้เหมาะกับแอปพลิเคชันที่ความเร็วและความมีประสิทธิภาพเป็นสิ่งสำคัญ

จากอดีตถึงปัจจุบัน logging ใน Go ได้พัฒนาอย่างมากตั้งแต่เริ่มต้นใช้ภาษา รุ่นแรกของ Go มอบความสามารถในการ logging พื้นฐานที่เราเห็นในแพคเกจ `log` อย่างไรก็ตาม เมื่อภาษาเติบโตในความนิยมและความซับซ้อนของแอปพลิเคชันที่เขียนด้วย Go เพิ่มขึ้น ชุมชนจึงเริ่มพัฒนาไลบรารี logging ที่ซับซ้อนขึ้นเพื่อตอบสนองความต้องการของพวกเขา ในวันนี้ แม้ว่าแพคเกจ `log` มาตรฐานยังคงเป็นตัวเลือกที่ดีสำหรับแอปพลิเคชันที่เรียบง่าย แต่หลายนักพัฒนาเทิร์นหันมาใช้โซลูชั่นของบุคคลที่สามสำหรับความต้องการการ logging ที่ซับซ้อนกว่า
