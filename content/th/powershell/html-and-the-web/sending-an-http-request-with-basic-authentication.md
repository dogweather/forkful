---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:19.781062-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E4C\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\
  \u0E19 \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E15\u0E34\u0E14\u0E15\u0E48\
  \u0E2D\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\
  \u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E41\u0E25\u0E30\u0E1A\u0E2D\u0E01\u0E27\u0E48\u0E32\
  \ \"\u0E19\u0E35\u0E48 \u0E09\u0E31\u0E19\u0E19\u0E30\" \u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.441175-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E4C\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\
  \u0E19 \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E15\u0E34\u0E14\u0E15\u0E48\
  \u0E2D\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\
  \u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E41\u0E25\u0E30\u0E1A\u0E2D\u0E01\u0E27\u0E48\u0E32\
  \ \"\u0E19\u0E35\u0E48 \u0E09\u0E31\u0E19\u0E19\u0E30\" \u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การส่งคำขอ HTTP ด้วยการตรวจสอบสิทธิ์แบบพื้นฐาน เป็นเมื่อโปรแกรมของคุณติดต่อกับเว็บเซิร์ฟเวอร์และบอกว่า "นี่ ฉันนะ" โดยใช้ชื่อผู้ใช้และรหัสผ่าน โปรแกรมเมอร์ใช้วิธีนี้เพื่อเข้าถึง API หรือทรัพยากรที่ต้องการหลักฐานของตัวตน - เหมือนกับการทำท่าทีลับที่ช่วยให้คุณเข้าสู่สโมสรได้

## วิธีการ:

นี่คือวิธีที่คุณขอข้อมูลจากเซิร์ฟเวอร์อย่างสุภาพด้วยคำว่า 'please' ในรูปแบบของการตรวจสอบสิทธิ์พื้นฐาน:

```PowerShell
# เตรียมข้อมูลประจำตัว
$user = 'YourUsername'
$pass = 'YourPassword'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# ตั้งค่าส่วนหัว
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# URL ที่คุณเคาะประตู
$url = 'https://api.example.com/data'

# ตอนนี้ เรามาทำตามขั้นตอนกัน
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# แสดงผลลัพธ์
$response
```

ตัวอย่างผลลัพธ์อาจมีลักษณะดังนี้ เมื่อสมมติว่าการตอบสนองเป็นรูปแบบ JSON:

```json
{
    "name": "John Doe",
    "email": "john@example.com"
}
```

## ศึกษาลึก

การตรวจสอบสิทธิ์แบบพื้นฐานนั้นเก่าแก่ ย้อนกลับไปถึงยุคแรกๆ ของอินเทอร์เน็ต ที่ทุกคนรู้จักกันทุกคน แม้ว่าจะยังถูกใช้งานอยู่ แต่มันไม่ปลอดภัยมากเมื่ออยู่คนเดียว - มันเหมือนกับการส่งรหัสลับของคุณในโปสการ์ด ในปัจจุบัน เรามักจะส่งมันผ่าน HTTPS เพื่อเข้ารหัส ซึ่งเหมือนกับการใส่โปสการ์ดนั้นในกล่องที่ล็อกไว้

มีทางเลือกอื่นไหม? มากมาย คุณมีคีย์ API, OAuth, โทเค็นเบียร์... รายการทำไปเรื่อย แต่ละอย่างมาพร้อมกับการจับมือและคำลับของตัวเอง

ในแง่ของการดำเนินการด้วย PowerShell, คุณกำลังแปลงชื่อผู้ใช้และรหัสผ่านของคุณเป็นรูปแบบที่โปรโตคอล HTTP เข้าใจได้ - base64 แต่จำไว้ว่า base64 ไม่ใช่การเข้ารหัส; มันเพียงแค่เป็นข้อความที่เล่นกับการปลอมแปลง เว้นแต่ว่าจะถูกส่งผ่าน HTTPS มิฉะนั้น ใครก็ตามที่แอบดูสามารถเปิดเผยมันได้

## ดูเพิ่มเติม

- [เอกสาร Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [การตรวจสอบสิทธิ์การเข้าถึงแบบพื้นฐานของ HTTP บน MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [ความเข้าใจเกี่ยวกับการเข้ารหัส Base64](https://en.wikipedia.org/wiki/Base64)
- [ข้อมูลเกี่ยวกับการเข้ารหัส HTTPS](https://en.wikipedia.org/wiki/HTTPS)
