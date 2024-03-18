---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:17.729338-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E08\u0E32\u0E01\
  \u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 Go \u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\
  \u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C, API \u0E2B\u0E23\u0E37\
  \u0E2D\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49 HTTP\
  \ \u0E2D\u0E37\u0E48\u0E19 \u0E46\u2026"
lastmod: '2024-03-17T21:57:55.668130-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E08\u0E32\u0E01\
  \u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 Go \u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\
  \u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C, API \u0E2B\u0E23\u0E37\
  \u0E2D\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49 HTTP\
  \ \u0E2D\u0E37\u0E48\u0E19 \u0E46\u2026"
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การส่งคำขอ HTTP หมายถึงการเริ่มต้นการเรียกจากแอปพลิเคชัน Go ของคุณไปยังเว็บเซิร์ฟเวอร์, API หรือบริการที่ใช้ HTTP อื่น ๆ โปรแกรมเมอร์ทำสิ่งนี้เพื่อโต้ตอบกับทรัพยากรเว็บ, ดึงข้อมูล, ส่งแบบฟอร์ม หรือสื่อสารกับบริการอื่น ๆ ทั่วอินเทอร์เน็ต

## วิธีการ:

ใน Go, การส่งคำขอ HTTP และการจัดการคำตอบเกี่ยวข้องกับการใช้แพ็คเกจ `net/http` นี่คือตัวอย่างทีละขั้นตอนในการส่งคำขอ GET แบบง่ายและอ่านคำตอบ:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // กำหนด URL ของทรัพยากร
    url := "http://example.com"

    // ใช้ http.Get เพื่อส่งคำขอ GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // ปิดตัวอ่านข้อมูลเมื่อฟังก์ชันสิ้นสุด
    defer resp.Body.Close()

    // อ่านตัวอ่านข้อมูลคำตอบ
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // แปลงตัวอ่านข้อมูลคำตอบเป็นสตริงและพิมพ์มัน
    fmt.Println(string(body))
}
```

ตัวอย่างผลลัพธ์ (ย่อเพื่อความกระชับ):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

เพื่อส่งคำขอ POST พร้อมข้อมูลฟอร์ม คุณสามารถใช้ `http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // กำหนด URL และข้อมูลฟอร์ม
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // ส่งคำขอ POST พร้อมข้อมูลฟอร์ม
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // อ่านและพิมพ์คำตอบ
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## การศึกษาเพิ่มเติม

แพ็คเกจ `net/http` ใน Go ให้วิธีการที่ทรงพลังและยืดหยุ่นในการโต้ตอบกับเซิร์ฟเวอร์ HTTP การออกแบบของมันสะท้อนถึงการเน้นความเรียบง่าย, ประสิทธิภาพ, และความแข็งแกร่งของ Go เมื่อเริ่มต้น ฟังก์ชั่นการจัดการข้อมูล JSON หรือ XML ต้องสร้างร่างข้อมูลคำขอด้วยตนเองและตั้งค่าส่วนหัวที่เหมาะสม ตามที่ Go พัฒนาขึ้น ชุมชนได้พัฒนาแพ็คเกจระดับสูงขึ้นที่ทำให้งานเหล่านี้ง่ายขึ้น เช่น `gorilla/mux` สำหรับการกำหนดเส้นทาง และ `gjson` สำหรับการจัดการ JSON

หนึ่งในแง่มุมที่น่าสังเกตของไคลเอนต์ HTTP ของ Go คือการใช้ interfaces และ structs เช่น `http.Client` และ `http.Request` ซึ่งทำให้สามารถปรับแต่งและทดสอบได้อย่างกว้างขวาง เช่น คุณสามารถปรับเปลี่ยน `http.Client` เพื่อหมดเวลาคำขอหรือรักษาการเชื่อมต่อเพื่อประสิทธิภาพ

ทางเลือกที่พิจารณาสำหรับการโต้ตอบ HTTP ที่ง่ายกว่าคือการใช้ไลบรารีบุคคลที่สาม เช่น "Resty" หรือ "Gentleman" แพ็คเกจเหล่านี้เสนอการแสดงความคิดในระดับที่สูงขึ้นสำหรับคำขอ HTTP, ทำให้งานทั่วไปง่ายขึ้น อย่างไรก็ตาม การเข้าใจและใช้งานแพ็คเกจ `net/http` ที่อยู่ภายใต้เป็นสิ่งสำคัญสำหรับการจัดการกับสถานการณ์การโต้ตอบ HTTP ที่ซับซ้อนหรือเป็นเอกลักษณ์ ซึ่งเป็นพื้นฐานที่คุณลักษณะของการทำงานพร้อมกันของ Go และไลบรารีมาตรฐานที่ทรงพลังสามารถถูกใช้ประโยชน์ได้อย่างเต็มที่
