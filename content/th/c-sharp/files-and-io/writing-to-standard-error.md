---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:10.126781-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E25\u0E07\u0E43\u0E19 standard error (stderr) \u0E43\u0E19\
  \ C# \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\
  \u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E1B\u0E01\u0E15\u0E34 (stdout)\u2026"
lastmod: '2024-03-17T21:57:56.242567-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E25\u0E07\u0E43\u0E19 standard error (stderr) \u0E43\u0E19\
  \ C# \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\
  \u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E1B\u0E01\u0E15\u0E34 (stdout)\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การเขียนข้อความลงใน standard error (stderr) ใน C# เกี่ยวข้องกับการแยกข้อความข้อผิดพลาดและการวินิจฉัยออกจากผลลัพธ์ปกติ (stdout) เพื่อช่วยให้ผู้ใช้และนักพัฒนาสามารถแยกแยะระหว่างผลลัพธ์โปรแกรมปกติกับการแจ้งเตือนข้อผิดพลาดได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อทำให้การดีบักและการบันทึกล็อกมีประสิทธิภาพมากขึ้น ช่วยให้การดำเนินการและการบำรุงรักษาแอปพลิเคชันเป็นไปได้อย่างราบรื่น

## วิธีการ:
ใน C# การเขียนข้อความลงใน standard error สามารถทำได้โดยใช้สตรีม `Console.Error` สตรีมนี้ใช้เฉพาะสำหรับข้อความข้อผิดพลาดและการวินิจฉัย นี่คือตัวอย่างพื้นฐาน:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

ผลลัพธ์ตัวอย่าง (ไปที่ stderr):
```
Error: Failed to process the request.
```

สำหรับสถานการณ์ที่คุณอาจใช้ไลบรารีของบุคคลที่สามที่มีฟังก์ชันการบันทึกล็อกขั้นสูง เช่น `Serilog` หรือ `NLog` คุณสามารถตั้งค่าไลบรารีเหล่านี้เพื่อเขียนบันทึกข้อผิดพลาดลงใน stderr ได้ แม้ว่าตัวอย่างเหล่านี้จะเน้นย้ำไปที่การเปลี่ยนทิศทางคอนโซลเรียบง่าย แต่จำไว้ว่าในแอปพลิเคชันระดับการผลิต กรอบการทำงานการบันทึกข้อมูลมีเครื่องมือในการจัดการและเลือกตัวเลือกผลลัพธ์ข้อผิดพลาดที่แข็งแกร่งกว่าเยอะ นี่คือตัวอย่างง่ายๆ กับ `Serilog`:

ก่อนอื่น ติดตั้งแพ็กเกจ Serilog และ Console sink ของมัน:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

จากนั้น ตั้งค่า Serilog เพื่อเขียนไปที่ stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

ผลลัพธ์ตัวอย่าง (ไปที่ stderr สำหรับข้อความข้อผิดพลาด):
```
[15:04:20 ERR] This is an error message.
```

หมายเหตุ: การตั้งค่า `standardErrorFromLevel` ใน console sink ของ Serilog จะทำการเปลี่ยนทิศทางกิจกรรมบันทึกข้อมูลที่ระดับที่ระบุ (ข้อผิดพลาดในกรณีนี้) หรือสูงกว่าไปยังสตรีม standard error ในขณะที่ข้อความระดับต่ำกว่า เช่น Information จะถูกเขียนไปยังสตรีมของผลลัพธ์ปกติ
