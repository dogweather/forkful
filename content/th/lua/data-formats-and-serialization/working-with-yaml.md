---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:46.441109-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Lua \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19 YAML \u0E43\
  \u0E19\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\
  \ YAML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \ \u0E40\u0E0A\u0E48\u0E19 `lyaml`\u2026"
lastmod: '2024-03-17T21:57:56.374388-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\
  \u0E2A\u0E19\u0E38\u0E19 YAML \u0E43\u0E19\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\
  \u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C YAML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\
  \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19 `lyaml` \u0E44\u0E25\
  \u0E1A\u0E23\u0E32\u0E23\u0E35\u0E19\u0E35\u0E49\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\
  \u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\
  \u0E2A\u0E41\u0E25\u0E30\u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\u0E2A\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25 YAML \u0E01\u0E31\u0E1A Lua \u0E44\u0E14\u0E49 \u0E01\u0E48\u0E2D\
  \u0E19\u0E2D\u0E37\u0E48\u0E19\u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E15\u0E34\
  \u0E14\u0E15\u0E31\u0E49\u0E07 `lyaml` \u0E1C\u0E48\u0E32\u0E19 LuaRocks \u0E15\u0E31\
  \u0E27\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E1E\u0E47\u0E01\u0E40\u0E01\u0E08\
  \u0E02\u0E2D\u0E07 Lua."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
Lua ไม่มีการสนับสนุน YAML ในตัว แต่คุณสามารถทำงานกับไฟล์ YAML โดยใช้ไลบรารีของบุคคลที่สาม เช่น `lyaml` ไลบรารีนี้ช่วยให้สามารถเข้ารหัสและถอดรหัสข้อมูล YAML กับ Lua ได้ ก่อนอื่นคุณต้องติดตั้ง `lyaml` ผ่าน LuaRocks ตัวจัดการแพ็กเกจของ Lua:

```bash
luarocks install lyaml
```

### การถอดรหัส YAML:
สมมติว่าคุณมีเนื้อหา YAML ต่อไปนี้ในไฟล์ที่ชื่อ `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

คุณสามารถถอดรหัสไฟล์ YAML นี้เข้าสู่ตาราง Lua โดยใช้โค้ดต่อไปนี้:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

เมื่อคุณเรียกใช้สคริปต์นี้ มันควรจะแสดงผล:

```output
host: localhost
port: 3306
username: user
password: pass
```

### การเข้ารหัส YAML:
เพื่อเข้ารหัสตาราง Lua เป็นรูปแบบ YAML คุณใช้ฟังก์ชัน `dump` ที่ `lyaml` ให้มา โดยคิดถึงว่าคุณต้องการสร้างการเป็นตัวแทน YAML ของตาราง Lua ต่อไปนี้:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

ผลลัพธ์ YAML จะเป็น:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

โดยการปฏิบัติตามรูปแบบเหล่านี้ โปรแกรมเมอร์ Lua สามารถจัดการข้อมูล YAML สำหรับแอปพลิเคชั่นต่างๆ ได้อย่างมีประสิทธิภาพ การดำเนินการเหล่านี้กับ YAML มีความสำคัญสำหรับการพัฒนาแอปพลิเคชั่น Lua ที่หลากหลายซึ่งสามารถทำงานร่วมกับส่วนอื่นๆ ของระบบหรือกับระบบอื่นๆ โดยตรงได้อย่างราบรื่น
