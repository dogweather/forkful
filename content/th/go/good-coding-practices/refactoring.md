---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:20.892270-06:00
description: "\u0E01\u0E32\u0E23 Refactoring \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\
  \u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E2D\u0E22\u0E39\u0E48\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E14\u0E34\u0E21\u2014\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E27\
  \u0E32\u0E07\u0E1F\u0E31\u0E01\u0E40\u0E15\u0E2D\u0E23\u0E4C\u2014\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \u2026"
lastmod: '2024-03-17T21:57:55.681349-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactoring \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\
  \u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E2D\u0E22\u0E39\u0E48\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E14\u0E34\u0E21\u2014\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E27\
  \u0E32\u0E07\u0E1F\u0E31\u0E01\u0E40\u0E15\u0E2D\u0E23\u0E4C\u2014\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E38\u0E07\u0E04\u0E27\
  \u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14 \u0E25\u0E14\u0E04\u0E27\u0E32\u0E21\u0E0B\
  \u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19 \u0E41\u0E25\u0E30\u0E40\u0E1E\u0E34\u0E48\u0E21\
  \u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E1A\u0E33\u0E23\u0E38\u0E07\u0E23\u0E31\u0E01\u0E29\u0E32 \u0E17\u0E49\u0E32\
  \u0E22\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E0B\u0E2D\
  \u0E1F\u0E15\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\u0E41\u0E01\
  \u0E49\u0E44\u0E02."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
ใน Go, Refactoring สามารถครอบคลุมตั้งแต่การปรับแต่งโค้ดเล็กๆ ไปจนถึงการเปลี่ยนแปลงที่ซับซ้อนกว่า มาเริ่มต้นด้วยตัวอย่างพื้นฐาน: การปรับปรุงฟังก์ชัน Go เริ่มต้นให้มีความเรียบง่ายและมีประสิทธิภาพที่ดีขึ้น

**ก่อน Refactoring:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // ผลลัพธ์: 59.9
}
```

**หลัง Refactoring:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // ผลลัพธ์: 59.9
}
```

ในเวอร์ชันที่ได้รับการ Refactoring คำสั่ง `else` ถูกลบออก ซึ่งทำให้การไหลของฟังก์ชันง่ายขึ้นโดยที่ไม่ส่งผลกระทบต่อผลลัพธ์—ตัวอย่างหนึ่งของเทคนิค Refactoring พื้นฐานแต่มีผลกระทบใน Go

สำหรับตัวอย่างที่ซับซ้อนขึ้น ลองพิจารณาการ Refactoring ฟังก์ชันเพื่อใช้ interfaces เพื่อความน่าใช้งานและการทดสอบที่ดีขึ้น:

**ก่อน Refactoring:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // จินตนาการถึงการประมวลผลข้อมูลที่นี่
    logger.Log("Data processed")
}

func main() {
    logger := Logger{}
    ProcessData("example data", logger)
}
```

**หลัง Refactoring:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // การประมวลผลข้อมูลยังคงเหมือนเดิม
    logger.Log("Data processed")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("example data", logger)
}
```

การ Refactoring เพื่อใช้ interface (`Logger`) แทนประเภทยูทิลิตี้ (`ConsoleLogger`) ปรับปรุงความยืดหยุ่นของฟังก์ชันและแยกการประมวลผลข้อมูลออกจากการใช้งานการบันทึกข้อมูลแบบเจาะจง

## ศึกษาลึก
การ Refactoring ใน Go ต้องจัดการความสัมพันธ์ระหว่างความเรียบง่าย (หนึ่งในหลักการหลักของ Go) กับความยืดหยุ่นที่จำเป็นสำหรับโปรเจ็กต์ซอฟต์แวร์ขนาดใหญ่ ด้วยความที่ Go มีความเรียบง่ายในการใช้งานฟีเจอร์—โดยไม่มี generics (จนกระทั่งเมื่อเร็วๆ นี้) และมีความสำคัญต่อความสามารถในการอ่าน—ภาษานี้เป็นทิศทางธรรมชาติให้นักพัฒนาเลือกโครงสร้างโค้ดที่เรียบง่ายและสามารถบำรุงรักษาได้ง่าย อย่างไรก็ตาม นี่ไม่ได้หมายความว่าโค้ด Go จะไม่ได้รับประโยชน์จากการ Refactoring; มันหมายความว่าการ Refactoring ต้องมีความชัดเจนและความเรียบง่ายเป็นหลัก

ในอดีต การขาดฟีเจอร์บางอย่างของ Go (เช่น generics ก่อน Go 1.18) ทำให้ต้องพึ่งพาการแก้ไขที่สร้างสรรค์แต่บางครั้งก็ซับซ้อนสำหรับการใช้โค้ดส่วนกลางและความยืดหยุ่น ทำให้การ Refactoring เพื่อการแยกส่วนเป็นปฏิบัติการทั่วไป ด้วยการเพิ่มฟีเจอร์ generics ใน Go 1.18 นักพัฒนา Go ตอนนี้กำลัง Refactoring โค้ดเก่าเพื่อใช้ประโยชน์จากฟีเจอร์นี้สำหรับความปลอดภัยของประเภทและการใช้โค้ดซ้ำที่ดีขึ้น แสดงให้เห็นถึงธรรมชาติการพัฒนาของการปฏิบัติการ Refactoring ใน Go

อย่างไรก็ตาม เครื่องมือของ Go รวมถึง `gofmt` สำหรับการจัดรูปแบบโค้ดและ `go vet` สำหรับการระบุโครงสร้างที่น่าสงสัย ช่วยให้สามารถรักษาฐานโค้ดที่สะอาดได้ ลดความจำเป็นในการ Refactoring อย่างกว้างขวาง ในขณะที่การ Refactoring เป็นเครื่องมือที่มีค่าไม่พอใจในอาวุธของโปรแกรมเมอร์ Go การใช้ฟีเจอร์และเครื่องมือของภาษา Go ตั้งแต่แรกสามารถช่วยลดความจำเป็นในการทำ Refactoring ที่ซับซ้อนในภายหลังได้
