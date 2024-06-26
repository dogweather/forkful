---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:29.142433-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Python \u0E21\u0E35\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\u0E48\u0E17\u0E23\u0E07\u0E1E\u0E25\
  \u0E31\u0E07\u0E40\u0E0A\u0E48\u0E19 BeautifulSoup \u0E41\u0E25\u0E30 requests \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.759945-06:00'
model: gpt-4-0125-preview
summary: "Python \u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\
  \u0E48\u0E17\u0E23\u0E07\u0E1E\u0E25\u0E31\u0E07\u0E40\u0E0A\u0E48\u0E19 BeautifulSoup\
  \ \u0E41\u0E25\u0E30 requests \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\
  \u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C HTML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\
  \u0E21\u0E15\u0E49\u0E19 \u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E15\u0E34\u0E14\
  \u0E15\u0E31\u0E49\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E40\u0E2B\u0E25\
  \u0E48\u0E32\u0E19\u0E35\u0E49\u0E16\u0E49\u0E32\u0E04\u0E38\u0E13\u0E22\u0E31\u0E07\
  \u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E17\u0E33."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
Python มีไลบรารีที่ทรงพลังเช่น BeautifulSoup และ requests สำหรับการเก็บข้อมูลจากเว็บและการวิเคราะห์ HTML เพื่อเริ่มต้น คุณต้องติดตั้งไลบรารีเหล่านี้ถ้าคุณยังไม่ได้ทำ:

```bash
pip install beautifulsoup4 requests
```

นี่คือตัวอย่างพื้นฐานในการใช้ `requests` เพื่อดึงเนื้อหา HTML ของเว็บเพจและใช้ `BeautifulSoup` เพื่อวิเคราะห์มัน:

```python
import requests
from bs4 import BeautifulSoup

# ดึงเนื้อหาของเว็บเพจ
URL = 'https://example.com'
page = requests.get(URL)

# วิเคราะห์เนื้อหา HTML
soup = BeautifulSoup(page.content, 'html.parser')

# ตัวอย่างการดึงชื่อของหน้าเว็บ
title = soup.find('title').text
print(f'ชื่อของเว็บเพจ: {title}')
```

**ตัวอย่างผลลัพธ์**:
```
ชื่อของเว็บเพจ: Example Domain
```

สำหรับการค้นหาที่ซับซ้อนกว่า เช่น การดึงลิงก์ทั้งหมดจากเว็บเพจ คุณสามารถใช้วิธีต่างๆ ของ BeautifulSoup ในการเดินทางและค้นหาระบบมีข้อมูล:

```python
# ดึงลิงก์ทั้งหมดภายในแท็ก <a>
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**ตัวอย่างผลลัพธ์**:
```
https://www.iana.org/domains/example
```

ความยืดหยุ่นของ BeautifulSoup ช่วยให้คุณสามารถปรับเปลี่ยนการค้นหาข้อมูลที่ต้องการได้อย่างเฉพาะเจาะจง ทำให้การวิเคราะห์ HTML เป็นเครื่องมือที่ทรงพลังสำหรับโปรแกรมเมอร์ที่ทำงานกับเนื้อหาเว็บ
