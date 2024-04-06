---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:29.848019-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML \u0E43\u0E19 TypeScript \u0E42\u0E14\
  \u0E22\u0E1B\u0E01\u0E15\u0E34\u0E08\u0E30\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 YAML \u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E47\u0E2D\
  \u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C JavaScript \u0E41\u0E25\u0E30\u0E2D\u0E32\u0E08\
  \u0E17\u0E33\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2D\u0E47\u0E2D\u0E1A\u0E40\
  \u0E08\u0E01\u0E15\u0E4C JavaScript \u0E01\u0E25\u0E31\u0E1A\u0E40\u0E1B\u0E47\u0E19\
  \u2026"
lastmod: '2024-03-17T21:57:55.965623-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML\
  \ \u0E43\u0E19 TypeScript \u0E42\u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\u0E08\u0E30\
  \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 YAML\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C\
  \ JavaScript \u0E41\u0E25\u0E30\u0E2D\u0E32\u0E08\u0E17\u0E33\u0E01\u0E32\u0E23\u0E41\
  \u0E1B\u0E25\u0E07\u0E2D\u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C JavaScript\
  \ \u0E01\u0E25\u0E31\u0E1A\u0E40\u0E1B\u0E47\u0E19 YAML \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\
  \u0E23\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E25\u0E07; \u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E37\u0E2D\u0E01\u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21\u0E2B\u0E19\u0E36\u0E48\
  \u0E07\u0E04\u0E37\u0E2D `js-yaml`, \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\
  \u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E27\u0E21\u0E40\u0E02\u0E49\
  \u0E32\u0E01\u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C TypeScript\
  \ \u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\n\n\u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\
  \u0E48\u0E19, \u0E40\u0E1E\u0E34\u0E48\u0E21 `js-yaml` \u0E40\u0E02\u0E49\u0E32\u0E44\
  \u0E1B\u0E43\u0E19\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
การทำงานกับ YAML ใน TypeScript โดยปกติจะเกี่ยวข้องกับการแปลงเนื้อหา YAML เป็นอ็อบเจกต์ JavaScript และอาจทำการแปลงอ็อบเจกต์ JavaScript กลับเป็น YAML การทำเช่นนี้ต้องการตัวแปลง; ตัวเลือกยอดนิยมหนึ่งคือ `js-yaml`, ไลบรารีที่สามารถรวมเข้ากับโปรเจกต์ TypeScript ได้ง่าย

### ติดตั้ง js-yaml
ก่อนอื่น, เพิ่ม `js-yaml` เข้าไปในโปรเจกต์ของคุณ:

```bash
npm install js-yaml
```

### การแปลง YAML เป็นอ็อบเจกต์ JavaScript
ลองจินตนาการว่าคุณมีไฟล์ YAML `config.yaml` ที่มีเนื้อหาดังนี้:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

คุณสามารถอ่านและแปลงไฟล์นี้เป็นอ็อบเจกต์ JavaScript ดังนี้:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// โหลดและแปลงไฟล์ YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**ผลลัพธ์ตัวอย่าง:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### การแปลงอ็อบเจกต์ JavaScript เป็น YAML
หากคุณต้องการทำในทางตรงกันข้าม คือ แปลงอ็อบเจกต์ JavaScript ไปเป็นสตริง YAML คุณสามารถใช้ `js-yaml` ดังนี้:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**ผลลัพธ์ตัวอย่าง:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

ส่วนตัวอย่างนี้แปลงอ็อบเจกต์ JavaScript ไปเป็นสตริง YAML และแสดงผลออกมา ในการปฏิบัติจริง คุณอาจจะเขียนกลับลงไปในไฟล์หรือใช้ในส่วนอื่นๆ ของแอปพลิเคชันได้
