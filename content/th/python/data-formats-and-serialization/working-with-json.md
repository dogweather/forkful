---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:58.107690-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E08\u0E31\u0E14\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E14\u0E49\u0E27\u0E22 JSON \u0E40\u0E1B\u0E47\
  \u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E43\u0E19 Python \u0E41\u0E25\u0E30\u0E43\
  \u0E19\u0E17\u0E32\u0E07\u0E01\u0E25\u0E31\u0E1A\u0E01\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.783989-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ (JavaScript Object Notation) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E08\u0E31\u0E14\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E14\u0E49\u0E27\u0E22 JSON \u0E40\u0E1B\u0E47\
  \u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E43\u0E19 Python \u0E41\u0E25\u0E30\u0E43\
  \u0E19\u0E17\u0E32\u0E07\u0E01\u0E25\u0E31\u0E1A\u0E01\u0E31\u0E19 \u0E19\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E40\
  \u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30 API \u0E40\u0E1E\u0E23\u0E32\u0E30 JSON \u0E40\
  \u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E01\u0E25\u0E32\u0E07\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\
  \u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E41\u0E25\
  \u0E30\u0E44\u0E04\u0E25\u0E40\u0E2D\u0E19\u0E15\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## อะไร & ทำไม?

การทำงานกับ JSON (JavaScript Object Notation) หมายถึงการแปลงข้อมูลจากสตริงที่ถูกจัดรูปแบบด้วย JSON เป็นวัตถุใน Python และในทางกลับกัน นี่เป็นสิ่งสำคัญสำหรับการพัฒนาเว็บและ API เพราะ JSON เป็นภาษากลางสำหรับการแลกเปลี่ยนข้อมูลระหว่างเซิร์ฟเวอร์และไคลเอนต์

## วิธีการ:

ไลบรารี `json` ที่มาพร้อมกับ Python ทำให้กระบวนการเข้ารหัส (การแปลงวัตถุ Python เป็น JSON) และถอดรหัส (การแปลง JSON เป็นวัตถุ Python) ง่ายขึ้น นี่คือวิธีที่คุณสามารถใช้มันได้:

### การเข้ารหัสวัตถุ Python เป็น JSON:

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**ผลลัพธ์:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### การถอดรหัส JSON เป็นวัตถุ Python:

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**ผลลัพธ์:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### การทำงานกับไลบรารีของบุคคลที่สาม:

สำหรับการจัดการ JSON ที่ซับซ้อน เช่น การตรวจสอบโครงสร้างหรือการแปลงไฟล์ JSON โดยตรงจาก URL ไลบรารีเช่น `requests` สำหรับการร้องขอ HTTP และ `jsonschema` สำหรับการตรวจสอบสามารถช่วยได้

#### ตัวอย่างด้วย `requests` เพื่อแปลงข้อมูล JSON จาก URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

สแนปช็อตนี้ดึงข้อมูล JSON จาก URL ที่กำหนดและแปลงโดยตรงเป็นวัตถุ Python

#### การใช้เครื่องมือ `jsonschema` เพื่อตรวจสอบ JSON:

ก่อนอื่นติดตั้งไลบรารีผ่าน pip:

```bash
pip install jsonschema
```

จากนั้นใช้ตามนี้:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# โดยสมมุติว่า `data` เป็นพจนานุกรมที่ได้มาจากการถอดรหัส JSON
try:
    validate(instance=data, schema=schema)
    print("ข้อมูล JSON ถูกต้อง")
except jsonschema.exceptions.ValidationError as err:
    print("ข้อผิดพลาดในการตรวจสอบ:", err)
```

ตัวอย่างนี้ทำการตรวจสอบพจนานุกรม Python ของคุณ (ที่ได้มาจากการถอดรหัสข้อมูล JSON) กับโครงสร้างที่กำหนดไว้ล่วงหน้า เพื่อให้แน่ใจว่าข้อมูลสอดคล้องกับรูปแบบและประเภทที่คาดหวัง
