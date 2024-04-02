---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:00.397005-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E43\u0E19 Go \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\
  \u0E23\u0E2B\u0E31\u0E2A\u0E41\u0E25\u0E30\u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\u0E2A\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E42\
  \u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E02\u0E2D\u0E07 Go \u0E41\u0E25\u0E30\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A JSON\u2026"
lastmod: '2024-03-17T21:57:55.694634-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E43\u0E19 Go \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\
  \u0E23\u0E2B\u0E31\u0E2A\u0E41\u0E25\u0E30\u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\u0E2A\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E42\
  \u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E02\u0E2D\u0E07 Go \u0E41\u0E25\u0E30\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A JSON\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## อะไรและทำไม?

การทำงานกับ JSON (JavaScript Object Notation) ใน Go เกี่ยวข้องกับการเข้ารหัสและถอดรหัสข้อมูลระหว่างโครงสร้างข้อมูลของ Go และรูปแบบ JSON งานนี้เป็นที่แพร่หลายในเว็บเซอร์วิสและ API เนื่องจาก JSON ให้บริการเป็นรูปแบบการแลกเปลี่ยนข้อมูลที่เบาและเป็นอิสระจากภาษา ทำให้การแบ่งปันข้อมูลเรียบง่ายต่อการใช้งานต่างๆ

## วิธีการ:

ใน Go, แพ็กเกจ `encoding/json` คือวิธีการเข้าถึงการจัดการ JSON ให้กลไกในการแปลงโครงสร้างข้อมูล Go เป็น JSON (การมาร์แชลลิ่ง) และกลับคืน (การอันมาร์แชลลิ่ง) ด้านล่างคือตัวอย่างเบื้องต้นในการเริ่มต้น:

### การเข้ารหัส (มาร์ชัลลิ่ง)

ในการแปลงโครงสร้าง Go เป็น JSON คุณสามารถใช้ `json.Marshal` พิจารณาโครงสร้าง Go ต่อไปนี้:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

ผลลัพธ์:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### การถอดรหัส (อันมาร์ชัลลิ่ง)

ในการแปลง JSON เป็นโครงสร้างข้อมูล Go ใช้ `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

กำหนดโครงสร้าง `User` เช่นเดิม โค้ดนี้จะแปลงสตริง JSON เป็นอินสแตนซ์ของ User

ผลลัพธ์:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## ลงลึก

แพ็กเกจ `encoding/json` ใน Go มอบ API ที่เรียบง่ายและคลายความซับซ้อนในการจัดการ JSON มาตั้งแต่ช่วงต้นของการพัฒนา Go แพ็กเกจนี้สะท้อนถึงปรัชญาของ Go ที่เน้นความเรียบง่ายและประสิทธิภาพ อย่างไรก็ตามการใช้การสะท้อน (reflection) โดย `encoding/json` เพื่อตรวจสอบและแก้ไขโครงสร้างข้อมูลในระหว่างเวลาทำงานอาจนำไปสู่ประสิทธิภาพที่ไม่เหมาะสมในสถานการณ์ที่ใช้ CPU มาก

ทางเลือกอย่าง `json-iterator/go` และ `ffjson` ได้ปรากฏขึ้นมา เพื่อให้การประมวลผล JSON เร็วขึ้นโดยการสร้างโค้ดมาร์แชลลิ่งและอันมาร์แชลลิ่งที่คงที่ อย่างไรก็ตาม `encoding/json` ยังคงเป็นแพ็กเกจที่ใช้กันทั่วไปเนื่องจากความเรียบง่ายความแข็งแกร่งและเป็นส่วนหนึ่งของไลบรารีมาตรฐาน ซึ่งรับประกันความเข้ากันได้และความเสถียรในรุ่นของ Go

แม้ว่าจะมีประสิทธิภาพช้ากว่าเมื่อเปรียบเทียบ ความง่ายในการใช้งานและการรวมกับระบบประเภทข้อมูลของ Go ทำให้ `encoding/json` เหมาะสำหรับแอปพลิเคชันส่วนใหญ่ สำหรับผู้ที่ทำงานในบริบทที่ประสิทธิภาพมีความสำคัญ การสำรวจไลบรารีภายนอกอาจคุ้มค่า แต่สำหรับหลายคน ไลบรารีมาตรฐานมีความสมดุลที่เหมาะสมระหว่างความเร็ว ความเรียบง่าย และความน่าเชื่อถือ
