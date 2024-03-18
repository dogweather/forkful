---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:22.251574-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 C# \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E08\u0E32\u0E01\u0E27\u0E31\u0E15\u0E16\u0E38 DateTime\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  \u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.237826-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 C# \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E08\u0E32\u0E01\u0E27\u0E31\u0E15\u0E16\u0E38 DateTime\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  \u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การแปลงวันที่เป็นสตริงใน C# คือการเปลี่ยนรูปแบบจากวัตถุ DateTime เป็นการแสดงผลแบบข้อความ โปรแกรมเมอร์ทำสิ่งนี้เพื่อแสดงวันที่ในรูปแบบที่เข้าใจง่ายสำหรับผู้ใช้หรือเพื่อ serialize ข้อมูลสำหรับการเก็บข้อมูลและการส่งข้อมูล

## วิธีการ:

ใน C#, คุณมีวัตถุ `DateTime` และหลายวิธีในการเปลี่ยนมันเป็นสตริง นี่คือตัวอย่างบางส่วน:

```csharp
DateTime now = DateTime.Now;
string defaultString = now.ToString(); // รูปแบบเริ่มต้น
string specificFormat = now.ToString("yyyy-MM-dd"); // รูปแบบที่กำหนดเอง, ที่นี้คือ ISO 8601
string withCulture = now.ToString("d", new CultureInfo("en-US")); // วัฒนธรรมของสหรัฐฯ รูปแบบวันที่สั้น

Console.WriteLine(defaultString); // ผลลัพธ์ขึ้นอยู่กับการตั้งค่าวัฒนธรรมของระบบ
Console.WriteLine(specificFormat); // ผลลัพธ์: "2023-04-01"
Console.WriteLine(withCulture); // ผลลัพธ์: "4/1/2023"
```

## ลงลึก

ก่อนหน้านี้, การจัดการเรื่องวันที่และสตริงนั้นยากกว่า ปัจจุบัน, `DateTime` ใน C# มี `.ToString()` ที่มี overloads สำหรับวัฒนธรรมและรูปแบบ `IFormatProvider` interface, เช่น `CultureInfo`, ควบคุมการจัดรูปแบบตามวัฒนธรรม

มีทางเลือกอื่นหรือไม่? แน่นอน! `String.Format` และ interpolation (`$"{now:yyyy-MM-dd}"`) เป็นตัวเลือกสำหรับการแทรกวันที่เข้ากับสตริงที่มีบริบท `DateTimeOffset` เป็นที่นิยมสำหรับความจำเพาะของเขตเวลา

จากมุมมองของการทำงาน, จำไว้ว่า `DateTime` เป็นโครงสร้าง, ดังนั้นจึงเป็นประเภทของค่า การแปลงมันไม่เปลี่ยนแปลงตัวตนต้นฉบับ: ความเป็น immutable สำคัญที่สุด เลือกรูปแบบสตริงอย่างชาญฉลาดตามผู้ชมของคุณ (ผู้ใช้) และระบบที่คุณกำลังส่งข้อมูลไปยัง (ฐานข้อมูล, APIs)

## ดูเพิ่มเติม

- [DateTime.ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
