---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:19.122800-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E41\u0E1A\u0E1A\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19 \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E43\u0E2A\u0E48\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  \u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E25\u0E07\u0E43\
  \u0E19\u0E04\u0E33\u0E02\u0E2D\u0E17\u0E35\u0E48\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E22\
  \u0E31\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.761905-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E41\u0E1A\u0E1A\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19 \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E43\u0E2A\u0E48\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  \u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E25\u0E07\u0E43\
  \u0E19\u0E04\u0E33\u0E02\u0E2D\u0E17\u0E35\u0E48\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E22\
  \u0E31\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## มันคืออะไรและทำไม?

การส่งคำขอ HTTP พร้อมกับการตรวจสอบสิทธิ์แบบพื้นฐาน หมายถึงการใส่ชื่อผู้ใช้และรหัสผ่านลงในคำขอที่ส่งไปยังเซิร์ฟเวอร์ เพื่อพิสูจน์ว่าคุณได้รับอนุญาตให้เข้าใช้ นักพัฒนาทำเช่นนี้เพื่อโต้ตอบกับ API หรือบริการเว็บที่ถูกล็อกอยู่เบื้องหลังการตรวจสอบสิทธิ์ผ่านข้อมูลผู้ใช้

## วิธีการ:

นี่คือวิธีที่คุณสามารถใช้ Python เพื่อสื่อสารกับเซิร์ฟเวอร์โดยใช้ Basic Auth

```Python
import requests
from requests.auth import HTTPBasicAuth

# แทนที่ด้วยข้อมูลรับรองจริงของคุณและ API endpoint ที่คุณต้องการเข้าถึง
username = 'cooluser'
password = 'supersecretpassword'
url = 'https://api.someservice.com/data'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

# ลองดูว่าเราได้อะไรกลับมา
print(response.status_code)
print(response.json())  # สมมติว่าการตอบกลับอยู่ในรูปแบบ JSON
```

ผลลัพธ์อาจจะดูเหมือนนี้ถ้าทุกอย่างราบรื่น:

```
200
{'data': 'ของลับของคุณ!'}
```

แต่ถ้าคุณกรอกข้อมูลรับรองผิด:

```
401
```

นั่นคือสัญญาณห้ามเข้าอย่างชัดเจนเลยนะ

## ศึกษาลึกซึ้ง

ด้วยประวัติศาสตร์แล้ว, HTTP Basic Auth เป็นวิธีที่เก่าแก่ที่สุดสำหรับความปลอดภัยบนเว็บ วิธีง่ายๆ ในการทำความรู้จักกับเว็บไซต์ มันไม่ค่อยปลอดภัยถ้าใช้คนเดียวเพราะมันส่งข้อมูลรับรองในรูปแบบข้อความธรรมดา แค่เข้ารหัสเป็น base64 - ไม่ใช่การเข้ารหัส ควรใช้ HTTPS เพื่อป้องกันไม่ให้ข้อมูลรับรองง่ายๆ ถูกรวบรวมเหมือนลูกอมจากเด็ก

มีวิธีการที่ปลอดภัยกว่าอย่าง เช่น Digest Access Authentication ที่รหัสผ่านไม่ถูกส่งในรูปแบบธรรมดาไปยังเครือข่าย OAuth เป็นอีกหนึ่งวิธีที่ใหญ่โต โดยเฉพาะสำหรับ API ในปัจจุบัน มันเหมือนกับการออกบัตรผ่านชั่วคราวมากกว่าการแสดงบัตรประจำตัวทุกครั้ง

ภายใต้ฝาครอบ, ไลบรารี `requests` กำลังเข้ารหัสชื่อผู้ใช้และรหัสผ่านของคุณและวางไว้ในหัวข้อ `Authorization` ที่จัดรูปแบบเหมือน `Basic base64encodedcredentials` เซิร์ฟเวอร์จะถอดรหัสหัวข้อนี้ เช็คข้อมูลรับรองของคุณ และถ้าคุณเป็นผู้ใช้ที่ถูกต้อง จะอนุญาตให้คุณเข้าถึง

## ดูเพิ่มเติม

- คู่มืออย่างเป็นทางการของไลบรารี `requests` ให้ข้อมูลเชิงลึกเกี่ยวกับ auth และอื่นๆ: https://docs.python-requests.org/en/latest/
- `http.client` สำหรับคนที่ต้องการทำงานโดยไม่ใช้ไลบรารีภายนอก: https://docs.python.org/3/library/http.client.html
- Real Python แนะนำเบื้องต้นเกี่ยวกับ HTTP และ Python: https://realpython.com/python-requests/
