---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:59.073775-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35 `requests` \u0E02\u0E2D\u0E07 Python \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01 HTTP \u0E40\u0E1B\u0E47\u0E19\
  \u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E14\u0E49\u0E32\u0E19\
  \u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D GET \u0E41\u0E1A\u0E1A\u0E07\u0E48\
  \u0E32\u0E22\u0E46."
lastmod: '2024-03-17T21:57:55.758759-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `requests` \u0E02\u0E2D\u0E07\
  \ Python \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\
  \u0E01 HTTP \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\
  \u0E32\u0E22 \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D\
  \ GET \u0E41\u0E1A\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
ไลบรารี `requests` ของ Python ทำให้การเรียก HTTP เป็นเรื่องง่าย ด้านล่างเป็นวิธีการส่งคำขอ GET แบบง่ายๆ:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # แสดงสถานะโค้ดของการตอบกลับ
print(response.json())      # หากการตอบกลับมีข้อมูล JSON, พิมพ์มันออกมาเป็น dict ของ Python
```

คำขอ POST ที่ละเอียดยิ่งขึ้นพร้อมโหลดข้อมูล JSON และส่วนหัวที่กำหนดเอง:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## ดำดิ่งลึกลงไป
คำขอ HTTP เป็นวิธีที่เว็บทำงาน — มีมาตั้งแต่ต้นยุค 90 ทางเลือกอื่นๆ ของไลบรารี `requests` ของ Python รวมถึงไลบรารีมาตรฐาน `urllib`, แต่มันมีความยุ่งยากมากขึ้นสักหน่อย

การเข้าใจการส่งคำขอ HTTP เกี่ยวข้องกับการรู้เรื่องวิธีการ (GET, POST, PUT, DELETE ฯลฯ), รหัสสถานะ (เช่น 200 OK, 404 Not Found), ส่วนหัว และข้อมูลในส่วนเนื้อหา

สำหรับคำขอแบบสตรีมหรือแบบไม่ใช้เวลาจริง คุณอาจสำรวจไลบรารีที่เป็นคู่ของ `requests` แบบ async หรือแพคเกจ `aiohttp` ข้างใต้ ไลบรารีเหล่านี้ใช้ `socket` ของ Python สำหรับการสื่อสารเครือข่ายดิบ

ทางประวัติศาสตร์แล้ว, `requests` ถูกพิจารณาเป็นตัวเลือกแรกเนื่องจากความเรียบง่ายและมีพลัง แต่ `httpx`, ไลบรารีที่เข้ากันได้กับ async ใหม่, กำลังได้รับความนิยม

## ดูเพิ่มเติม
- เอกสารคู่มือของไลบรารี `requests`: https://requests.readthedocs.io
- รหัสสถานะ HTTP ที่อธิบายไว้: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- เอกสารคู่มือของ Python `urllib`: https://docs.python.org/3/library/urllib.html
- ไลบรารี `httpx` สำหรับคำขอ HTTP แบบ async: https://www.python-httpx.org
