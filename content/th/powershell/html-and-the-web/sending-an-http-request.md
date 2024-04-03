---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:51.369431-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D GET \u0E17\u0E35\u0E48\u0E07\
  \u0E48\u0E32\u0E22."
lastmod: '2024-03-17T21:57:56.438276-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\
  \u0E32\u0E22\u0E46 \u0E43\u0E19\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\
  \u0E2D GET \u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
นี่คือวิธีง่ายๆ ในการส่งคำขอ GET ที่ง่าย:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

และถ้าหากคุณต้องการ POST ข้อมูลบางอย่าง:

```PowerShell
$body = @{
    'name' = 'Jane Doe'
    'occupation' = 'Space Ranger'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

ตัวอย่างผลลัพธ์:

```
name         occupation
----         ----------
Jane Doe     Space Ranger
```

## ข้อมูลเพิ่มเติม:
การส่งคำขอ HTTP ย้อนกลับไปถึงจุดเริ่มต้นของการพัฒนาเว็บ คุณกำลังมีส่วนร่วมในการสนทนากับเว็บในภาษาที่เป็นปฐมภูมิของมัน, HTTP. คำสั่ง `Invoke-RestMethod` ใน PowerShell เป็นเครื่องมือที่เลือกใช้ในที่นี้ ก่อนหน้า `Invoke-RestMethod`, `Invoke-WebRequest` เป็นคำสั่งที่ไปถึงและยังคงมีอยู่สำหรับการตอบกลับที่ละเอียดยิ่งขึ้น

ถ้าคุณรู้สึกชอบการผจญภัย, คุณมีทางเลือกอื่นเช่น `curl` หรือคลาส `HttpClient` ของ .NET เมื่อใช้ `Invoke-RestMethod`, จำไว้ว่ามันคือ wrapper รอบคลาสและเมธอดของ `HttpClient` ใน .NET, ซึ่งมอบความสะดวกแต่แลกมาด้วยการควบคุมระดับล่างที่บางอย่าง

ในแง่ของการเตรียมการ, จำไว้ว่าคำขอ HTTP มาพร้อมกับวิธีการต่างๆ เช่น `GET`, `POST`, `PUT`, เป็นต้น คุณสามารถกำหนดหัวข้อด้วย `-Headers`, และจัดการเวลาหมดเขตและการรับรองความถูกต้องด้วยพารามิเตอร์เพิ่มเติมตามที่ต้องการ ถ้าคุณใช้เนื้อหาที่สร้างโดยผู้ใช้, จำเป็นต้อง sanitize ข้อมูลเสมอเพื่อหลีกเลี่ยงการโจมตีผ่านการแทรกสคริปต์

## ดูเพิ่มเติม:
- [เกี่ยวกับ Invoke-RestMethod ของ PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [รายละเอียด `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [ความเข้าใจเกี่ยวกับ REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [คลาส `.NET HttpClient`](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
