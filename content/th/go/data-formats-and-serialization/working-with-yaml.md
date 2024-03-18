---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:02.876026-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML\
  \ \u0E43\u0E19 Go \u0E21\u0E35\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E44\u0E1F\u0E25\u0E4C YAML (YAML Ain't Markup\
  \ Language) \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E44\u0E25\
  \u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\
  \u0E19\u0E21\u0E34\u0E15\u0E23\u0E01\u0E31\u0E1A\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\
  \u2026"
lastmod: '2024-03-17T21:57:55.693451-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML\
  \ \u0E43\u0E19 Go \u0E21\u0E35\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E44\u0E1F\u0E25\u0E4C YAML (YAML Ain't Markup\
  \ Language) \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E44\u0E25\
  \u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\
  \u0E19\u0E21\u0E34\u0E15\u0E23\u0E01\u0E31\u0E1A\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การทำงานกับ YAML ใน Go มีการแยกวิเคราะห์ไฟล์ YAML (YAML Ain't Markup Language) ซึ่งเป็นมาตรฐานการซีเรียไลซ์ข้อมูลที่เป็นมิตรกับมนุษย์ เข้ากับโครงสร้างข้อมูลของ Go และในทางกลับกัน โปรแกรมเมอร์ทำเช่นนี้เพื่อใช้ประโยชน์จากความง่ายและความสามารถอ่านได้ของ YAML สำหรับไฟล์การกำหนดค่า, การตั้งค่าแอปพลิเคชัน หรือการแลกเปลี่ยนข้อมูลระหว่างบริการและส่วนประกอบที่เขียนด้วยภาษาที่แตกต่างกัน

## วิธีการ:

เพื่อทำงานกับ YAML ใน Go คุณจะต้องนำเข้าไลบรารีที่รองรับการวิเคราะห์และการซีเรียไลซ์ YAML เนื่องจากไลบรารีมาตรฐานของ Go ไม่รวมการรองรับสำหรับ YAML โดยตรง ไลบรารีที่นิยมมากที่สุดสำหรับวัตถุประสงค์นี้คือ "gopkg.in/yaml.v3" นี่คือวิธีการเริ่มต้น:

1. **การติดตั้งแพ็กเกจ YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **การแยกวิเคราะห์ YAML เข้าสู่โครงสร้าง Go:**

ก่อนอื่น กำหนดโครงสร้างใน Go ที่ตรงกับโครงสร้างข้อมูล YAML ของคุณ

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**ตัวอย่างผลลัพธ์:**

```
User: admin
Password: secret
```

3. **การซีเรียไลซ์โครงสร้าง Go เป็น YAML:**

นี่คือวิธีการแปลงโครงสร้าง Go กลับเป็น YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**ตัวอย่างผลลัพธ์:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## ดำดิ่งลึก:

การใช้ YAML ในการพัฒนาซอฟต์แวร์ได้เพิ่มขึ้นเนื่องจากมีรูปแบบที่เข้าใจง่าย ทำให้เหมาะสมกับไฟล์การกำหนดค่า, เอกสาร, หรือรูปแบบการแลกเปลี่ยนข้อมูล เมื่อเทียบกับ JSON ที่เป็นคู่แข่ง YAML มอบความคิดเห็น, ประเภทสเกลาร์ และคุณสมบัติความสัมพันธ์ ให้กรอบการซีเรียไลซ์ข้อมูลที่ร่ำรวยกว่า อย่างไรก็ตาม ความยืดหยุ่นและคุณสมบัติต่างๆ นั้นต้องแลกมาด้วยความซับซ้อนในการแยกวิเคราะห์ นำไปสู่ความเสี่ยงด้านความปลอดภัยเมื่อไม่ระมัดระวังอย่างเพียงพอ (เช่น การดำเนินการโค้ดที่ไม่คาดคิด)

ไลบรารี "gopkg.in/yaml.v3" สำหรับ Go เป็นโซลูชันที่แข็งแกร่งสำหรับการประมวลผล YAML ที่ทำให้การใช้งานง่ายและการรองรับคุณสมบัติที่ครบถ้วนเป็นสมดุล ณ สถานะปัจจุบัน ในขณะที่มีทางเลือกอื่นๆ เช่น "go-yaml/yaml" (ไลบรารีที่อยู่เบื้องหลัง "gopkg.in/yaml.v3") การเลือกเวอร์ชันโดยปกติแล้วขึ้นอยู่กับความต้องการของโปรเจกต์หรือความชอบส่วนบุคคล เมื่อจัดการกับชุดข้อมูลขนาดใหญ่หรือแอปพลิเคชันที่เน้นประสิทธิภาพ โปรแกรมเมอร์อาจพิจารณาใช้รูปแบบที่เรียบง่ายอย่าง JSON เนื่องจากมีเวลาและหน่วยความจำในการแยกวิเคราะห์ที่น้อยกว่า อย่างไรก็ตาม สำหรับไฟล์การกำหนดค่าหรือการตั้งค่าที่ความสามารถในการอ่านได้ง่ายและความสะดวกในการใช้งานเป็นสิ่งสำคัญ YAML ยังคงเป็นตัวเลือกที่แข็งแกร่งในระบบนิเวศ Go
