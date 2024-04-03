---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:53.028912-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E19\u0E1A\
  \u0E19\u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\u0E4C \u0E42\u0E14\u0E22\u0E01\u0E32\
  \u0E23\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API, \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u2026"
lastmod: '2024-03-17T21:57:56.222816-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E19\u0E1A\
  \u0E19\u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\u0E4C \u0E42\u0E14\u0E22\u0E01\u0E32\
  \u0E23\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API, \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23 \u0E2B\
  \u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E40\u0E19\u0E37\
  \u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## อะไร & ทำไม?
การส่งคำขอ HTTP เป็นวิธีที่โปรแกรมสื่อสารกันบนเว็บไซต์ โดยการขอข้อมูลหรือส่งข้อมูล โปรแกรมเมอร์ทำเช่นนี้เพื่อทำการโต้ตอบกับ API, บริการ หรือเพื่อดึงเนื้อหาเว็บ

## วิธีการ:
C# ทำให้การส่งคำขอ HTTP เป็นเรื่องง่ายด้วย `HttpClient` นี่คือโครงสร้างของคำขอ GET:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseBody);
    }
}
```

ตัวอย่างผลลัพธ์ (ตัดทอน):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## ลงลึก
`HttpClient` ถูกนำมาใช้ใน .NET Framework 4.5 เพื่อทำให้การสื่อสาร HTTP เป็นเรื่องง่ายยิ่งขึ้น ก่อนหน้านั้น คุณอาจต้องรับมือกับคลาส `HttpWebRequest` และ `HttpWebResponse` ซึ่งมีความยุ่งยากมากขึ้น

มีวิธีอื่นๆ ในการส่งคำขอ HTTP ใน C# `RestSharp` และ `Flurl` เป็นสองไลบรารีของบุคคลที่สามที่ได้รับความนิยม เสนออินเตอร์เฟซที่ใช้งานง่ายขึ้นและคุณสมบัติเพิ่มเติม แต่ `HttpClient` มักจะเพียงพอสำหรับความต้องการส่วนใหญ่

ในเรื่องการใช้งาน, `HttpClient` ถูกออกแบบมาให้สามารถใช้ซ้ำได้สำหรับหลายๆ คำขอ การสร้างอินสแตนซ์ใหม่สำหรับทุกคำขอสามารถทำให้จำนวนซ็อกเก็ตที่ใช้งานได้หมดลงภายใต้ภาระที่หนักหน่วง ต้องระมัดระวังเสมอ และฉันหมายความว่าเสมอ ในการดูแลการทิ้งอินสแตนซ์ของ `HttpClient` เพื่อหลีกเลี่ยงการรั่วไหลของทรัพยากร

## ดูเพิ่มเติม
- เอกสารของ Microsoft เกี่ยวกับ `HttpClient`: [https://docs.microsoft.com/th-th/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- ปฏิบัติการที่ดีที่สุดของ HttpClient: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- การโต้ตอบกับ RESTful API ด้วย `RestSharp`: [http://restsharp.org/](http://restsharp.org/)
- Fluent HTTP (HTTP ทำได้ง่าย) ด้วย `Flurl`: [https://flurl.dev/](https://flurl.dev/)
