---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:47.640054-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: **\u0E01\u0E32\u0E23\u0E41\
  \u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19:** `DateTime.Parse` \u0E41\u0E25\u0E30 `DateTime.TryParse`\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E44\
  \u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19 `DateTime` \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\
  \u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
lastmod: '2024-04-05T21:54:01.912812-06:00'
model: gpt-4-0125-preview
summary: "**\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19:** `DateTime.Parse`\
  \ \u0E41\u0E25\u0E30 `DateTime.TryParse` \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E37\u0E2D\u0E01\u0E44\u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\
  \u0E19 `DateTime` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:
**การแยกวิเคราะห์พื้นฐาน:**

`DateTime.Parse` และ `DateTime.TryParse` เป็นตัวเลือกไปสำหรับการแปลงสตริงเป็น `DateTime` นี่คือตัวอย่างที่รวดเร็ว:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"การแยกวิเคราะห์สำเร็จ: {parsedDate}");
}
else
{
    Console.WriteLine("การแยกวิเคราะห์ล้มเหลว");
}
// ผลลัพธ์: การแยกวิเคราะห์สำเร็จ: 4/12/2023 12:00:00 AM
```

**ระบุวัฒนธรรม:**

บางครั้งคุณต้องการแยกวิเคราะห์สตริงวันที่ที่อยู่ในรูปแบบวัฒนธรรมเฉพาะ คุณสามารถทำได้โดยใช้คลาส `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// ผลลัพธ์: 4/12/2023 12:00:00 AM
```

**การแยกวิเคราะห์แบบเฉพาะด้วยรูปแบบที่เจาะจง:**

สำหรับสถานการณ์ที่วันที่มาในรูปแบบที่อาจจะไม่เป็นมาตรฐาน, `DateTime.ParseExact` เป็นวิธีที่เหมาะสม:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// ผลลัพธ์: 4/12/2023 12:00:00 AM
```

**การใช้ NodaTime:**

สำหรับการแยกวิเคราะห์วันที่และเวลาที่แข็งแกร่งยิ่งขึ้น พิจารณาใช้ไลบรารีภายนอกยอดนิยมอย่าง NodaTime มันให้ความสามารถในการจัดการวันที่/เวลาที่กว้างขวางยิ่งขึ้น:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("การแยกวิเคราะห์ล้มเหลว");
}
```

NodaTime มีการสนับสนุนเวลาท้องถิ่น แนวคิดเรื่องระยะเวลาและช่วงเวลา และระบบปฏิทินที่แตกต่างกันมากมาย ทำให้เป็นตัวเลือกที่มีประสิทธิภาพสำหรับการจัดการวันที่และเวลาที่ซับซ้อนในแอปพลิเคชัน .NET
