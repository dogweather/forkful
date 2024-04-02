---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:15.394168-06:00
description: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\
  \u0E13\u0E4C\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  \u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E2D\u0E1B\u0E1E\u0E25\
  \u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\
  \u0E23\u0E31\u0E19\u0E44\u0E17\u0E21\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\
  \u0E36\u0E01\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E1A\u0E31\u0E4A\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.233042-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\
  \u0E13\u0E4C\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  \u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E2D\u0E1B\u0E1E\u0E25\
  \u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\
  \u0E23\u0E31\u0E19\u0E44\u0E17\u0E21\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\
  \u0E36\u0E01\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E1A\u0E31\u0E4A\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## การบันทึกข้อมูลการทำงานและเหตุผลที่ต้องทำ
การบันทึกเป็นกระบวนการในการบันทึกเหตุการณ์และการแสดงผลของข้อมูลแอปพลิเคชันในขณะที่รันไทม์ โปรแกรมเมอร์ทำการบันทึกเพื่อวินิจฉัยบั๊ก, ติดตามประสิทธิภาพซอฟต์แวร์, ติดตามการกระทำของผู้ใช้ และรักษาการปฏิบัติตามมาตรฐานด้านความปลอดภัยและธุรกิจ

## วิธีการ:
ใน C#, คุณสามารถใช้ `System.Diagnostics` namespace ในตัวหรือไลบรารี่ของบุคคลที่สามเช่น NLog หรือ log4net นี่คือตัวอย่างที่รวดเร็วโดยใช้ `ILogger` interface ที่มีใน .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("นี่คือข้อความข้อมูล");
        logger.LogWarning("นี่คือข้อความเตือน");
        logger.LogError("นี่คือข้อความข้อผิดพลาด");
    }
}
```

ตัวอย่างผลลัพธ์:
```
info: Program[0]
      นี่คือข้อความข้อมูล
warn: Program[0]
      นี่คือข้อความเตือน
fail: Program[0]
      นี่คือข้อความข้อผิดพลาด
```

## การศึกษาเพิ่มเติม
ประวัติของการบันทึกในการพัฒนาซอฟต์แวร์นั้นเก่าแก่เกือบเท่ากับการเขียนโปรแกรมเอง; ได้พัฒนาจากคำสั่งพิมพ์ง่ายๆ ไปสู่ระบบที่สามารถปรับแต่งได้อย่างซับซ้อน ในต้นแต่การบันทึกจะทำโดยการเขียนลงในไฟล์หรือคอนโซล แต่ได้ขยายไปสู่โครงสร้างที่ซับซ้อนมากขึ้น เช่น ระบบรวมบันทึกและแพลตฟอร์มติดตามแบบกระจาย (เช่น ELK stack หรือ Jaeger).

ตัวเลือกที่นอกเหนือจากการบันทึกในตัวของ .NET ได้แก่ ไลบรารี่ของบุคคลที่สาม:
- **NLog**: หลากหลาย และง่ายต่อการตั้งค่า มีคุณสมบัติมากมายสำหรับการเส้นทาง, การจัดรูปแบบ และการกรองบันทึก
- **log4net**: ได้แรงบันดาลใจจากไลบรารี่ log4j ของ Java นี้สามารถปรับแต่งได้อย่างสูงจาก XML และรองรับหลายไร้พพอสิทอรีบันทึก

เมื่อพูดถึงรายละเอียดในการดำเนินการ การเลือกการสรุปการบันทึกของคุณ (เช่น Microsoft.Extensions.Logging) และผู้ให้บริการบันทึกข้อมูลภายใต้สามารถส่งผลอย่างมากต่อประสิทธิภาพและความน่าเชื่อถือของแอปพลิเคชันของคุณ มันสำคัญที่จะต้องกำหนดระดับการบันทึกอย่างเหมาะสมและให้แน่ใจว่าการเขียนบันทึกไม่เป็นจุดขัดข้อง

นอกจากนี้ การบันทึกโครงสร้าง - ซึ่งคุณบันทึกไม่เพียงแต่สตริง แต่ยังมีคู่คีย์-ค่าหรือวัตถุ - อนุญาตให้มีบันทึกที่แม่นยำและสามารถดำเนินการได้มากขึ้น ซึ่งง่ายต่อการค้นหาและวิเคราะห์

## ดูเพิ่มเติม
- [เอกสาร Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [เอกสาร NLog](https://nlog-project.org/documentation/)
- [เอกสาร log4net](https://logging.apache.org/log4net/)
- [เอกสาร Serilog](https://serilog.net/) (ตัวอย่างของการบันทึกโครงสร้าง)
