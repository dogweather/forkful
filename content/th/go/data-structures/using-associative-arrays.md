---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:33.492423-06:00
description: "\u0E41\u0E2D\u0E2A\u0E42\u0E0B\u0E0B\u0E34\u0E40\u0E2D\u0E17\u0E35\u0E1F\
  \u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\
  \u0E48\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E19 Go \u0E27\u0E48\u0E32\u0E41\u0E21\
  \u0E1E \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E40\u0E01\u0E47\u0E1A\u0E04\u0E39\u0E48\u0E04\u0E48\u0E32\u0E02\
  \u0E2D\u0E07\u0E01\u0E38\u0E0D\u0E41\u0E08-\u0E04\u0E48\u0E32 (key-value pairs)\u2026"
lastmod: '2024-03-17T21:57:55.663758-06:00'
model: gpt-4-0125-preview
summary: "\u0E41\u0E2D\u0E2A\u0E42\u0E0B\u0E0B\u0E34\u0E40\u0E2D\u0E17\u0E35\u0E1F\
  \u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\
  \u0E48\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E19 Go \u0E27\u0E48\u0E32\u0E41\u0E21\
  \u0E1E \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E40\u0E01\u0E47\u0E1A\u0E04\u0E39\u0E48\u0E04\u0E48\u0E32\u0E02\
  \u0E2D\u0E07\u0E01\u0E38\u0E0D\u0E41\u0E08-\u0E04\u0E48\u0E32 (key-value pairs)\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

แอสโซซิเอทีฟอาร์เรย์ หรือที่เรียกใน Go ว่าแมพ ช่วยให้คุณสามารถเก็บคู่ค่าของกุญแจ-ค่า (key-value pairs) โดยที่กุญแจที่ไม่ซ้ำกันแต่ละอันจะแมปไปยังค่าหนึ่งค่า โปรแกรมเมอร์ใช้แมพเพื่อการค้นหาข้อมูลที่มีประสิทธิภาพ, การเปลี่ยนแปลง, และเพื่อรักษาคอลเลกชันขององค์ประกอบที่สามารถเข้าถึงได้อย่างรวดเร็วโดยใช้กุญแจที่ไม่ซ้ำกัน

## วิธีการ:

การสร้างและเริ่มต้นใช้งานแมพใน Go สามารถทำได้หลายวิธี นี่คือตัวอย่างพื้นฐานเพื่อให้คุณเริ่มต้น:

```go
package main

import "fmt"

func main() {
    // การประกาศและเริ่มใช้งานแมพ
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Output: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

เพื่อเพิ่มหรืออัปเดตองค์ประกอบ, คุณกำหนดค่าให้กับกุญแจเช่นนี้:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Output: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

การเข้าถึงค่าด้วยกุญแจของมันเป็นเรื่องง่าย:

```go
fmt.Println("โค้ดสีหกเลขสำหรับสีแดงคือ:", colors["red"])
// Output: โค้ดสีหกเลขสำหรับสีแดงคือ: #FF0000
```

เพื่อลบองค์ประกอบออก, ใช้ฟังก์ชัน `delete`:

```go
delete(colors, "red")
fmt.Println(colors)
// Output: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

การวนซ้ำผ่านแมพทำได้โดยใช้ลูป for:

```go
for color, hex := range colors {
    fmt.Printf("Key: %s Value: %s\n", color, hex)
}
```

จำไว้ว่า, แมพใน Go เป็นไม่มีการเรียงลำดับ ลำดับของการวนซ้ำไม่ได้รับการรับประกัน

## ศึกษาลึกซึ้ง

ใน Go, แมพถูกดำเนินการเป็นตารางแฮช แต่ละรายการในแมพประกอบด้วยสองสิ่ง: กุญแจและค่า กุญแจถูกแฮชเพื่อเก็บรายการ, ซึ่งช่วยให้การดำเนินการใช้เวลาคงที่สำหรับชุดข้อมูลขนาดเล็กและความซับซ้อนเฉลี่ยของเวลา O(1) ด้วยการแฮชที่เหมาะสม, ซึ่งอาจเสื่อมสภาพเป็น O(n) ในกรณีที่แย่ที่สุดด้วยการชนของแฮชมากมาย

หมายเหตุสำคัญสำหรับโปรแกรมเมอร์ Go ใหม่คือประเภทแมพเป็นประเภทอ้างอิง นี้หมายความว่าเมื่อคุณผ่านแมพไปยังฟังก์ชัน การเปลี่ยนแปลงใด ๆ ที่ทำกับแมพภายในฟังก์ชันนั้นสามารถมองเห็นได้จากผู้เรียก นี่เป็นความแตกต่างจาก เช่น, การส่งโครงสร้างไปยังฟังก์ชัน, ซึ่งโครงสร้างจะถูกคัดลอกเว้นเสียแต่ว่าจะส่งโดยพอยน์เตอร์

ในขณะที่แมพนั้นมีความยืดหยุ่นและมีประสิทธิภาพสำหรับกรณีการใช้งานส่วนใหญ่ที่เกี่ยวข้องกับแอสโซซิเอทีฟอาร์เรย์, ในแอพพลิเคชั่นที่มีความต้องการประสิทธิภาพสูง อาจเป็นประโยชน์ที่จะใช้โครงสร้างข้อมูลที่มีลักษณะประสิทธิภาพที่สามารถคาดการณ์ได้มากขึ้นโดยเฉพาะเมื่อการกระจายกุญแจอาจทำให้เกิดการชนกันบ่อยครั้ง

ทางเลือกอื่นที่ควรพิจารณาคือ `sync.Map`, ซึ่งมีตั้งแต่ Go 1.9, ออกแบบมาสำหรับกรณีการใช้งานที่กุญแจถูกเขียนเพียงครั้งเดียวแต่ถูกอ่านหลายครั้ง, ซึ่งให้การปรับปรุงความมีประสิทธิภาพในสถานการณ์เหล่านี้ อย่างไรก็ตาม, สำหรับแอพพลิเคชั่น Go ทั่วไป, การใช้งานแมพแบบปกติถือเป็นสิ่งที่ถูกต้องตามไวยากรณ์และมักเป็นวิธีที่แนะนำสำหรับความเรียบง่ายและการสนับสนุนโดยตรงในภาษา