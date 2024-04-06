---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:44.234695-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19 Go, \u0E41\u0E1E\u0E04\
  \u0E40\u0E01\u0E08 `strings` \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E15\u0E48\u0E32\u0E07\u0E46\u0E21\u0E32\u0E01\u0E21\u0E32\u0E22\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E20\u0E32\u0E22\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E21\u0E32\u0E2A\
  \u0E33\u0E23\u0E27\u0E08\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E31\u0E48\
  \u0E27\u0E44\u0E1B\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E01\u0E31\u0E19\
  \ **\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `strings.Contains`\u2026"
lastmod: '2024-04-05T21:54:00.978336-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Go, \u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08 `strings` \u0E21\u0E35\
  \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E15\u0E48\u0E32\u0E07\u0E46\u0E21\
  \u0E32\u0E01\u0E21\u0E32\u0E22\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\
  \u0E32\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E20\u0E32\u0E22\u0E43\u0E19\u0E2A\
  \u0E15\u0E23\u0E34\u0E07 \u0E21\u0E32\u0E2A\u0E33\u0E23\u0E27\u0E08\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E1A\u0E32\u0E07\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E01\u0E31\u0E19 **\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\
  \ `strings.Contains` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21:**."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีทำ:
ใน Go, แพคเกจ `strings` มีฟังก์ชันต่างๆมากมายเพื่อค้นหาและเปลี่ยนแปลงข้อความภายในสตริง มาสำรวจวิธีการทั่วไปบางอย่างกัน

**การใช้ `strings.Contains` เพื่อค้นหาข้อความ:**

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go programmers!"
	fmt.Println(strings.Contains(myString, "Go"))  // Output: true
	fmt.Println(strings.Contains(myString, "Java")) // Output: false
}
```

**การเปลี่ยนแปลงข้อความด้วย `strings.Replace` และ `strings.ReplaceAll`:**

`strings.Replace` ช่วยให้คุณสามารถเปลี่ยนแปลงสับสตริงภายในสตริงได้ โดยระบุจำนวนการเปลี่ยนแปลงที่จะทำ ในขณะที่ `strings.ReplaceAll` เปลี่ยนทุกอินสแตนซ์

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go! Go is fun."
	fmt.Println(strings.Replace(myString, "Go", "Golang", 1))  // Output: Hello, Golang! Go is fun.
	fmt.Println(strings.ReplaceAll(myString, "Go", "Golang")) // Output: Hello, Golang! Golang is fun.
}
```

**การใช้แพคเกจ `regexp` สำหรับการค้นหาและเปลี่ยนแปลงข้อความขั้นสูง:**

สำหรับรูปแบบที่ซับซ้อนยิ่งขึ้น แพคเกจ `regexp` ทรงพลังมาก โดยสนับสนุนการใช้ regular expressions

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	myString := "Hello, Go programmers! Go is fun."
	re := regexp.MustCompile(`Go`)
	fmt.Println(re.ReplaceAllString(myString, "Golang"))  // Output: Hello, Golang programmers! Golang is fun.
}
```

## ค้นพบอย่างลึกซึ้ง
ใน Go, การจัดการข้อความ รวมถึงการดำเนินการค้นหาและเปลี่ยนแปลง ได้รับการออกแบบให้เป็นเรื่องง่ายและมีประสิทธิภาพ โดยใช้ประโยชน์จากไลบรารีมาตรฐานของ Go แพคเกจ `strings` มอบฟังก์ชันพื้นฐานที่เหมาะสมสำหรับกรณีการใช้งานทั่วไปในขณะที่แพคเกจ `regexp` รองรับรูปแบบที่ซับซ้อนขึ้นซึ่งต้องการการใช้ regular expressions.

จากประวัติศาสตร์ Go ได้เน้นไปที่ความเรียบง่ายและประสิทธิภาพในการจัดการสตริงและการดำเนินการจัดการข้อความ การตัดสินใจที่จะรวมแพคเกจทรงพลังเช่น `strings` และ `regexp` เป็นส่วนหนึ่งของไลบรารีมาตรฐานขับเนื่องจากความประสงค์ที่จะทำให้ Go เป็นตัวเลือกที่เหมาะสมสำหรับการพัฒนาเว็บและการประมวลผลข้อความ ซึ่งการดำเนินการเหล่านี้บ่อยครั้ง

ควรสังเกตว่าในขณะที่แพคเกจ `strings` และ `regexp` ของ Go ครอบคลุมขอบเขตความต้องการที่กว้างขวาง ยังมีสถานการณ์ที่ภาษาอื่นหรือไลบรารีเฉพาะอาจเสนอคุณสมบัติการจัดการข้อความที่ขั้นสูงขึ้น โดยเฉพาะอย่างยิ่งในด้านการจัดการ Unicode หรือการประมวลผลภาษาธรรมชาติ อย่างไรก็ตาม สำหรับงานค้นหาและเปลี่ยนแปลงในการพัฒนาซอฟต์แวร์ส่วนใหญ่ Go มอบเครื่องมือที่ทรงพลังและมีประสิทธิภาพออกมาจากกล่อง
