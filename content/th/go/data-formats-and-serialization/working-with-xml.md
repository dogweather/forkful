---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:37.731166-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E43\u0E19 Go \u0E19\u0E31\u0E49\u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (\u0E2D\u0E48\u0E32\u0E19) \u0E41\u0E25\
  \u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07 (\u0E40\u0E02\u0E35\u0E22\u0E19) \u0E40\u0E2D\
  \u0E01\u0E2A\u0E32\u0E23 XML\u2014\u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\
  \u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u2026"
lastmod: '2024-03-17T21:57:55.697528-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E43\
  \u0E19 Go \u0E19\u0E31\u0E49\u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\
  \u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (\u0E2D\u0E48\u0E32\u0E19) \u0E41\u0E25\u0E30\
  \u0E2A\u0E23\u0E49\u0E32\u0E07 (\u0E40\u0E02\u0E35\u0E22\u0E19) \u0E40\u0E2D\u0E01\
  \u0E2A\u0E32\u0E23 XML\u2014\u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\
  \u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การทำงานกับ XML ใน Go นั้นเกี่ยวข้องกับการแยกวิเคราะห์ (อ่าน) และสร้าง (เขียน) เอกสาร XML—ซึ่งเป็นรูปแบบมาตรฐานสำหรับการแลกเปลี่ยนข้อมูลที่มีโครงสร้าง โปรแกรมเมอร์ทำสิ่งนี้เพื่อการเก็บข้อมูล, การตั้งค่าคอนฟิก, หรือการแลกเปลี่ยนข้อมูลระหว่างระบบ โดยเฉพาะอย่างยิ่งในสภาพแวดล้อมที่ XML เป็นรูปแบบข้อมูลที่ต้องการหรือเป็น legacy

## วิธีการ:

### การแยกวิเคราะห์ XML ใน Go
เพื่อแยกวิเคราะห์ XML ใน Go คุณใช้แพ็คเกจ `encoding/xml` แพ็คเกจนี้ให้เครื่องมือที่จำเป็นในการ unmarshal (แยกวิเคราะห์) XML ให้เป็น structs ของ Go ตัวอย่างเช่น พิจารณาข้อมูล XML ต่อไปนี้ที่แทนหนังสือ:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

เพื่อแยกวิเคราะห์สิ่งนี้ ให้กำหนด struct ที่สะท้อนโครงสร้าง XML:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

ผลลัพธ์:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### การสร้าง XML ใน Go
เพื่อสร้างเอกสาร XML จากโครงสร้างข้อมูลของ Go คุณอีกครั้งใช้แพ็คเกจ `encoding/xml` คราวนี้คุณ marshal structs ของ Go เป็น XML โดยพิจารณา struct `Book` ก่อนหน้านี้:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

ผลลัพธ์:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## ด้านลึก

ความแออัดและความซับซ้อนของ XML ทำให้ JSON และรูปแบบอื่น ๆ ได้รับความนิยมมากขึ้นสำหรับแอปพลิเคชันหลายๆ อย่าง อย่างไรก็ตาม ความสามารถของ XML ในการแสดงข้อมูลเชิงลำดับชั้นที่ซับซ้อนและการใช้งานที่แพร่หลายในระบบเก่าและโดเมนเฉพาะ (เช่น บริการ SOAP) ยังคงรับประกันความเกี่ยวข้องของมัน

แพ็คเกจ `encoding/xml` ใน Go ให้กลไกที่ทรงพลังสำหรับการทำงานกับ XML แต่ก็คุ้มค่าที่จะทราบถึงข้อจำกัดของมัน ตัวอย่างเช่น การจัดการกับชื่อเนมสเปซ XML อาจเป็นเรื่องยุ่งยากและอาจต้องการความเข้าใจที่ละเอียดยิ่งขึ้นเกี่ยวกับข้อกำหนด XML มากกว่าในกรณีการใช้งานที่ง่ายกว่า นอกจากนี้ แม้ว่าการพิมพ์แบบคงที่ของ Go และความสามารถในการ marshal และ unmarshal ของแพ็คเกจ `encoding/xml` จะมีประสิทธิภาพโดยทั่วไป นักพัฒนาอาจพบเจอกับความท้าทายในโครงสร้างที่ซ้อนกันอย่างลึกหรือเมื่อต้องจัดการกับเอกสาร XML ที่ไม่ตรงกับระบบประเภทของ Go ได้อย่างเป็นระเบียบ

สำหรับแอปพลิเคชันสมัยใหม่ส่วนใหญ่ ทางเลือกเช่น JSON นั้นง่ายและมีประสิทธิภาพมากขึ้น อย่างไรก็ตาม เมื่อทำงานในบริบทที่ต้องการ XML—เนื่องจากระบบเก่า มาตรฐานอุตสาหกรรมเฉพาะ หรือความต้องการการแสดงข้อมูลที่ซับซ้อน—ไลบรารีมาตรฐานของ Go มีเครื่องมือที่แข็งแกร่งเพื่อให้งานสำเร็จ ตามปกติแล้ว การเลือกข้อมูลรูปแบบขึ้นอยู่กับข้อกำหนดเฉพาะของแอปพลิเคชันและสภาพแวดล้อม
