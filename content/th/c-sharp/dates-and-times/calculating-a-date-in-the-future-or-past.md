---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:54.443854-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E04\
  \u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\
  \u0E32\u0E04\u0E15."
lastmod: '2024-03-17T21:57:56.239693-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15."
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## วิธีการ:
การคำนวณวันที่ในอนาคต:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan oneWeek = TimeSpan.FromDays(7);
        
        DateTime nextWeek = currentDate + oneWeek;
        Console.WriteLine($"หนึ่งสัปดาห์จากนี้: {nextWeek}");
    }
}
```

ผลลัพธ์:

```
หนึ่งสัปดาห์จากนี้: <วันที่หนึ่งสัปดาห์จากวันปัจจุบัน>
```

การคำนวณวันที่ในอดีต:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan tenDaysAgo = TimeSpan.FromDays(-10);
        
        DateTime pastDate = currentDate + tenDaysAgo;
        Console.WriteLine($"สิบวันที่ผ่านมาคือ: {pastDate}");
    }
}
```

ผลลัพธ์:

```
สิบวันที่ผ่านมาคือ: <วันที่สิบวันก่อนวันปัจจุบัน>
```

## ศึกษาเพิ่มเติม
ใน C#, `DateTime` และ `TimeSpan` เป็นหัวใจหลักในการดำเนินการด้านวันที่และเวลา `DateTime` แทนการที่แน่นอนในเวลา, โดยปกติจะแสดงเป็นวันที่และเวลาของวัน ในขณะที่ `TimeSpan` แทนช่วงเวลา

ในอดีต, การคำนวณวันที่และเวลามักเกิดข้อผิดพลาดเนื่องจากการจัดการกับวัน, เดือน, และปีอธิกสุรทินด้วยตนเอง `DateTime` ทำให้ความซับซ้อนเหล่านี้แปลงเป็นรูปแบบง่าย ๆ โดยให้เฟรมเวิร์กจัดการกับส่วนที่ยาก

ทางเลือกของ `DateTime` และ `TimeSpan` ใน .NET รวมถึง `DateTimeOffset` ซึ่งรวมถึงการหักช่วงเวลาของเขตเวลา, ทำให้เหมาะกับแอปพลิเคชั่นที่ทำงานข้ามเขตเวลา ทางเลือกอื่นคือ Noda Time, ไลบรารีจาก Jon Skeet ออกแบบมาเพื่อการจัดการวันที่และเวลาที่ซับซ้อนยิ่งขึ้น เช่น ปฏิทินที่แตกต่างกัน

ในเชิงการดำเนินการ, เมื่อคุณเพิ่ม `TimeSpan` เข้ากับ `DateTime`, ในส่วนลึกของเฟรมเวิร์ก, มันกำลังจัดการกับขีด ซึ่งเป็นหน่วยพื้นฐานของเวลาใน .NET (`1 ขีด = 100 นาโนวินาที`) สำหรับวันที่ในอดีต, `TimeSpan` ที่มีค่าลบจะทำหน้าที่นี้ได้ดี

## ดูเพิ่มเติม
- เอกสารอ้างอิง API ของ .NET สำหรับ [`DateTime`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- บทนำเกี่ยวกับ [`TimeSpan`](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- แนวทางปฏิบัติที่ดีที่สุดของ Microsoft สำหรับ [`DateTime` และ `DateTimeOffset`](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- เอกสารของ Noda Time: [https://nodatime.org](https://nodatime.org)
