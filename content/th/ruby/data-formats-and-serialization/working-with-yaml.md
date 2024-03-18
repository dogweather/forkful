---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:36.149709-06:00
description: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E16\u0E39\u0E01\u0E43\u0E0A\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E01\
  \u0E27\u0E49\u0E32\u0E07\u0E02\u0E27\u0E32\u0E07\u0E43\u0E19 Ruby \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E25\
  \u0E44\u0E17\u0E21\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E19\u0E37\u0E48\
  \u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\
  \u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.748721-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E16\u0E39\u0E01\u0E43\u0E0A\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E01\
  \u0E27\u0E49\u0E32\u0E07\u0E02\u0E27\u0E32\u0E07\u0E43\u0E19 Ruby \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E25\
  \u0E44\u0E17\u0E21\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E19\u0E37\u0E48\
  \u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\
  \u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
YAML ย่อมาจาก YAML Ain't Markup Language ถูกใช้อย่างกว้างขวางใน Ruby สำหรับไฟล์การตั้งค่าและการเรียลไทม์ข้อมูลเนื่องจากมีรูปแบบที่อ่านได้ง่ายสำหรับมนุษย์ นักพัฒนามักเลือกใช้ YAML เมื่อพวกเขาต้องการเก็บหรือส่งข้อมูลออบเจ็กต์ในรูปแบบที่อ่านได้ง่ายแต่มีโครงสร้างรัดกุม ทำให้งานเช่นการจัดการการตั้งค่า การจัดเก็บข้อมูล และการแบ่งปันข้อมูลระหว่างภาษาต่างๆ เป็นเรื่องง่ายขึ้น

## วิธีการ:
Ruby มีไลบรารีภายในที่เรียกว่า Psych สำหรับการแยกวิเคราะห์และสร้าง YAML ใหม่ ในการใช้งาน คุณต้องเรียกใช้ไลบรารีมาตรฐานของ YAML ก่อน นี่คือตัวอย่างพื้นฐานเพื่อเริ่มต้น:

```ruby
require 'yaml'

# แฮชที่จะถูกเรียลไทม์
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# แปลงแฮชเป็น YAML
yaml_data = person.to_yaml

puts yaml_data
```

**ตัวอย่างผลลัพธ์:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

ในการโหลดข้อมูล YAML กลับเป็นออบเจ็กต์ Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**ตัวอย่างผลลัพธ์:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### การใช้ไลบรารีจากบุคคลที่สาม:

แม้ว่าไลบรารีมาตรฐานจะเพียงพอสำหรับงานพื้นฐาน แต่สำหรับความต้องการที่ซับซ้อนคุณอาจต้องการมองหาเจมส์จากบุคคลที่สาม เช่น 'safe_yaml' ในการใช้ไลบรารีเหล่านี้ คุณต้องติดตั้งเจมก่อน:

```bash
gem install safe_yaml
```

จากนั้น คุณสามารถใช้มันในการโหลดข้อมูล YAML อย่างปลอดภัย ลดความเสี่ยงเช่นการสร้างอินสแตนซ์วัตถุจากแหล่งควบคุมโดยผู้ใช้:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**ตัวอย่างผลลัพธ์:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

วิธีนี้ช่วยเพิ่มความปลอดภัยในการจัดการ YAML ของคุณ ทำให้เป็นตัวเลือกที่ดีสำหรับแอปพลิเคชันที่โหลด YAML จากแหล่งที่ไม่ไว้วางใจ
