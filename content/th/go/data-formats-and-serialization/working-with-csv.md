---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:52.875823-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C CSV \u0E43\u0E19\
  \ Go \u0E19\u0E31\u0E49\u0E19\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E14\u0E49\
  \u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19 `encoding/csv` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\
  \u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C CSV."
lastmod: '2024-04-05T21:54:01.053515-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV \u0E43\u0E19 Go \u0E19\u0E31\u0E49\u0E19\u0E07\u0E48\u0E32\
  \u0E22\u0E14\u0E32\u0E22 \u0E14\u0E49\u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19 `encoding/csv` \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\
  \u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C CSV."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
การทำงานกับไฟล์ CSV ใน Go นั้นง่ายดาย ด้วยไลบรารีมาตรฐาน `encoding/csv` นี่คือการเริ่มต้นอ่านและเขียนไฟล์ CSV

### การอ่านไฟล์ CSV
ในการอ่านจากไฟล์ CSV, คุณเริ่มต้นโดยเปิดไฟล์โดยใช้ `os.Open`, จากนั้นสร้าง CSV reader ใหม่ด้วย `csv.NewReader`

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

ส่วนของโค้ดนี้จะอ่านทุกระเบียนจาก `data.csv` และพิมพ์ออกมา แต่ละระเบียนเป็นสไลซ์ของฟิลด์

### การเขียนไปยังไฟล์ CSV
สำหรับการเขียน, คุณใช้ `csv.NewWriter` และ `writer.WriteAll` หรือ `writer.Write` สำหรับการเขียนหลายๆ หรือระเบียน CSV แบบเดียว, ตามลำดับ

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

สิ่งนี้จะสร้างไฟล์ที่ชื่อ `output.csv` ด้วยระเบียนที่ให้มา อย่าลืมใช้ flush กับ writer เพื่อให้ข้อมูลทั้งหมดถูกเขียนไปยังไฟล์

## การศึกษาลึกลงไป
แพ็คเกจ `encoding/csv` ของ Go ให้การสนับสนุนที่แข็งแกร่งสำหรับการอ่านและเขียนไฟล์ CSV แต่ถูกออกแบบมาด้วยความเรียบง่ายในใจ ซึ่งหมายความว่ามันไม่จัดการกับสถานการณ์ที่ซับซ้อนเช่น การตรวจจับ delimiters โดยอัตโนมัติ, การจัดการกับคำพูดหรือการหักบรรทัดในฟิลด์โดยไม่ต้องจัดการเอง

ในอดีต, การจัดการ CSV ในภาษาโปรแกรมมิ่งมักจะยุ่งยากเนื่องจากความซับซ้อนเหล่านี้ แต่ไลบรารีมาตรฐานของ Go ทำให้หลายๆ ปัญหาเหล่านี้เป็นเรื่องง่าย, ช่วยให้นักพัฒนาสามารถทำงานกับข้อมูล CSV ได้โดยง่าย อย่างไรก็ตาม, สำหรับการจัดการ CSV ที่ซับซ้อนมากขึ้น, อาจจำเป็นต้องใช้ไลบรารีภายนอกเช่น `gocsv` หรือการจัดการการแยกวิเคราะห์ด้วยตัวเอง

หนึ่งในจุดเด่นของแพ็คเกจ `csv` ของ Go คือการสนับสนุนให้กำหนด comma (delimiter) ที่กำหนดเองได้ ซึ่งช่วยให้สามารถทำงานได้อย่างราบรื่นกับรูปแบบไฟล์ CSV ต่างๆ เช่น ค่าที่แยกด้วยแท็บ (TSV) อย่างไรก็ตาม, เมื่อต้องจัดการกับไฟล์ CSV ที่ไม่เป็นมาตรฐานสูงหรือมีความไม่สม่ำเสมอ, โปรแกรมเมอร์ Go อาจพบว่าตัวเองต้องขยายการทำงานของ csv reader หรือ writer ที่มีอยู่

ถึงแม้ว่าความสามารถในการจัดการ CSV ของ Go จะแข็งแกร่งสำหรับจุดประสงค์ทั่วไป, สำหรับการใช้งานที่ต้องการการจัดการข้อมูลอย่างหนัก, เช่น งานวิทยาศาสตร์ข้อมูลหรืองานแปลงข้อมูลที่ซับซ้อน, โปรแกรมเมอร์อาจมองหาแพ็คเกจการประมวลผลข้อมูลเฉพาะหรือแม้กระทั่งภาษาอื่นที่เหมาะกับงานเหล่านี้, เช่น Python กับไลบรารี `pandas` อย่างไรก็ตาม, สำหรับการดำเนินการอ่าน-เขียน CSV ที่ตรงไปตรงมา, ไลบรารีมาตรฐานของ Go โดดเด่นด้วยประสิทธิภาพและความเรียบง่าย
