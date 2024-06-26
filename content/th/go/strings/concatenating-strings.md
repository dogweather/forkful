---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:55.370008-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Go \u0E21\u0E35\
  \u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\
  \u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E14\u0E39\u0E17\u0E35\u0E48\u0E1A\u0E32\u0E07\u0E27\u0E34\u0E18\u0E35\
  \u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E15\u0E31\u0E27\
  \u0E2D\u0E22\u0E48\u0E32\u0E07."
lastmod: '2024-04-05T21:54:00.992210-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Go \u0E21\u0E35\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E39\u0E17\u0E35\u0E48\u0E1A\
  \u0E32\u0E07\u0E27\u0E34\u0E18\u0E35\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E1E\u0E23\
  \u0E49\u0E2D\u0E21\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีการ:
ใน Go มีหลายวิธีในการต่อสตริง นี่คือการดูที่บางวิธีทั่วไปพร้อมตัวอย่าง:

### การใช้งาน `+` Operator:
วิธีที่ง่ายที่สุดในการต่อสตริงคือการใช้ `+` operator มันง่ายดายแต่ไม่ใช่วิธีที่มีประสิทธิภาพที่สุดสำหรับหลายสตริง
```go
firstName := "John"
lastName := "Doe"
fullName := firstName + " " + lastName
fmt.Println(fullName) // John Doe
```

### การใช้ `fmt.Sprintf`:
สำหรับการจัดรูปแบบสตริงด้วยตัวแปร `fmt.Sprintf` เป็นตัวเลือกที่ดีมาก มันให้การควบคุมเกี่ยวกับรูปแบบการแสดงผลมากขึ้น
```go
age := 30
message := fmt.Sprintf("%s is %d years old.", fullName, age)
fmt.Println(message) // John Doe is 30 years old.
```

### การใช้ `strings.Builder`:
สำหรับการต่อสตริงหลายตัว โดยเฉพาะในลูป `strings.Builder` เป็นวิธีที่มีประสิทธิภาพและแนะนำ
```go
var builder strings.Builder
words := []string{"hello", "world", "from", "go"}

for _, word := range words {
    builder.WriteString(word)
    builder.WriteString(" ")
}

result := builder.String()
fmt.Println(result) // hello world from go 
```

### การใช้ `strings.Join`:
เมื่อคุณมี slice ของสตริงที่ต้องการเชื่อมต่อด้วยตัวคั่นที่เฉพาะเจาะจง `strings.Join` เป็นตัวเลือกที่ดีที่สุด
```go
elements := []string{"path", "to", "file"}
path := strings.Join(elements, "/")
fmt.Println(path) // path/to/file
```

## ศึกษาลึกลงไป
การต่อสตริง แม้จะดูเป็นการดำเนินการที่ตรงไปตรงมา แต่ก็เกี่ยวข้องกับแง่มุมที่ลึกซึ้งยิ่งขึ้นเกี่ยวกับวิธีการจัดการสตริงของ Go ใน Go สตริงเป็น immutable หมายความว่า ทุกการดำเนินการต่อสตริงสร้างสตริงใหม่ขึ้นมา นี่อาจนำไปสู่ปัญหาประสิทธิภาพเมื่อต้องต่อสตริงจำนวนมากหรือเมื่อทำในลูปที่รัดกุม เนื่องจากการจัดสรรและการคัดลอกหน่วยความจำบ่อยครั้ง

ในอดีต ภาษาต่างๆ ได้จัดการกับความไม่เปลี่ยนแปลงของสตริงและประสิทธิภาพในการต่อสตริงในหลายวิธี และวิธีการของ Go กับ `strings.Builder` และ `strings.Join` ให้เครื่องมือแก่โปรแกรมเมอร์ที่ทำให้สามารถจัดสมดุลระหว่างความง่ายในการใช้งานและประสิทธิภาพได้ ประเภท `strings.Builder` ที่นำเสนอใน Go 1.10 เป็นสิ่งที่น่าสังเกตเพราะมันให้วิธีที่มีประสิทธิภาพในการสร้างสตริงโดยไม่มีภาระของการจัดสรรสตริงหลายตัว เป็นการจัดสรรบัฟเฟอร์ที่เติบโตตามความต้องการ ซึ่งสตริงจะถูกเพิ่มเข้าไป

แม้จะมีตัวเลือกเหล่านี้ แต่มันสำคัญมากที่จะเลือกวิธีที่เหมาะสมตามบริบท สำหรับการต่อสตริงอย่างรวดเร็วหรือไม่บ่อยครั้ง การดำเนินการง่ายๆ หรือ `fmt.Sprintf` อาจเพียงพอ อย่างไรก็ตาม สำหรับเส้นทางที่สำคัญต่อประสิทธิภาพ โดยเฉพาะที่มีการต่อสตริงหลายครั้ง การใช้ประโยชน์จาก `strings.Builder` หรือ `strings.Join` อาจเหมาะสมกว่า

แม้ว่า Go จะมีความสามารถในการจัดการสตริงที่แข็งแกร่งมาก แต่ก็จำเป็นต้องตระหนักถึงลักษณะประสิทธิภาพที่อยู่เบื้องหลังตลอดเวลา ทางเลือกเช่นการต่อสตริงผ่าน `+` หรือ `fmt.Sprintf` ให้บริการได้ดีสำหรับความเรียบง่ายและการดำเนินการเล็กๆ น้อยๆ แต่การเข้าใจและใช้ปฏิบัติการสร้างสตริงของ Go ที่มีประสิทธิภาพมากกว่า ช่วยให้แอปพลิเคชันของคุณยังคงมีประสิทธิภาพและสามารถขยายขนาดได้
