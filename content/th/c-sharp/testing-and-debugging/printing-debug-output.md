---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:27.273969-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E2A\u0E34\u0E48\u0E07\
  \u0E07\u0E48\u0E32\u0E22\u0E46: \u0E43\u0E0A\u0E49 `Console.WriteLine()` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E44\u0E1B\u0E17\u0E35\u0E48\u0E04\
  \u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25 \u0E42\u0E14\u0E22\
  \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E15\u0E23\u0E27\u0E08\u0E08\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\
  \u0E25\u0E32\u0E14, `Debug.WriteLine()`\u2026"
lastmod: '2024-04-05T21:54:01.901436-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
สิ่งง่ายๆ: ใช้ `Console.WriteLine()` เพื่อพิมพ์ไปที่คอนโซลแสดงผล โดยเฉพาะสำหรับการตรวจจับข้อผิดพลาด, `Debug.WriteLine()` สามารถเป็นตัวเลือกของคุณได้ หากคุณมี `System.Diagnostics` ในคำสั่งใช้ของคุณ หากคุณกำลังเป้าหมายไปที่แอปพลิเคชัน UI, `Trace.WriteLine()` อาจเป็นเครื่องมือสำหรับงานเนื่องจากมันอนุญาตให้ผู้ฟังจับการแสดงผลได้

```C#
using System;
using System.Diagnostics;

public class DebugExample
{
    public static void Main()
    {
        int magicNumber = 42;
        Console.WriteLine("สวัสดีทุกคน! มาตรวจสอบกันเถอะ");
        Debug.WriteLine($"ตัวเลขมหัศจรรย์คือ: {magicNumber}");

        // แกล้งทำเป็นว่าเรามีเงื่อนไขที่นี่
        Trace.WriteLine("เราอยู่ในเมทริกซ์!");
    }
}
```

คอนโซลแสดงผลจะดูเหมือน:
```
สวัสดีทุกคน! มาตรวจสอบกันเถอะ
```

การแสดงผลการตรวจแก้จุดบกพร่อง ที่มองเห็นได้ในหน้าต่างการแสดงผลการตรวจแก้จุดบกพร่องของ IDE หรือผู้ฟังของคุณ จะเป็น:
```
ตัวเลขมหัศจรรย์คือ: 42
เราอยู่ในเมทริกซ์!
```

## ตรวจสอบลึกๆ
มาย้อนเวลากัน ตอนที่ C# เป็นสิ่งใหม่ๆ คนตรวจจับข้อผิดพลาดด้วยกล่องข้อความ - จินตนาการถึงการคลิก 'OK' หลายร้อยครั้ง แต่เครื่องมือพัฒนาไป วิธี 'Console.WriteLine()' เป็นวิธีที่น่าเชื่อถือ, วิธีรวดเร็วในการพิมพ์ข้อมูลแสดงผล, เหมาะสมที่สุดสำหรับใช้ในแอพคอนโซล อย่างไรก็ตาม, เมื่อคุณได้พัฒนาจากแอพคอนโซลไปสู่การพัฒนาแอพ Windows Forms หรือ WPF เป็นต้น, 'Debug.WriteLine()' และ 'Trace.WriteLine()' จากเนมสเปซ `System.Diagnostics` กลายเป็นสิ่งที่น่าสนใจยิ่งขึ้น

'Debug.Writeline()' จะแสดงผลเฉพาะเมื่อการสร้างอยู่ในโหมด Debug; มันจะเงียบในโหมด Release ลักษณะการทำงานนี้ทำให้มันเรียบร้อยสำหรับการพิมพ์การตรวจจับข้อผิดพลาดชั่วคราวที่คุณไม่ต้องกังวลเกี่ยวกับการทำความสะอาดในภายหลัง ในทางกลับกัน, 'Trace.WriteLine()' สามารถเปิดใช้งานได้ทั้งในโหมด Debug และ Release ซึ่งสามารถช่วยติดตามปัญหาหลังจากการปรับใช้

ควรทราบว่า คุณสามารถใส่คำเรียก `Debug` และ `Trace` ไปในโค้ดของคุณ และคุณสามารถควบคุมการแสดงผลของพวกเขาโดยใช้ Listeners, โดยไม่ต้องคอมไพล์ใหม่ทุกครั้งที่คุณเปลี่ยนที่ที่การแสดงผลจะไป น่าสนใจ, ใช่ไหม?

## ดูเพิ่มเติม
สำหรับเสียงหัวเราะและความรู้เพิ่มเติม, ตรวจสอบลิงก์เหล่านี้:
- เอกสารอย่างเป็นทางการของ Microsoft เกี่ยวกับ `Debug`: [Debug Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- เอกสารอย่างเป็นทางการของ Microsoft เกี่ยวกับ `Trace`: [Trace Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace)
- การสำรวจลึกลงไปใน listeners และ trace sources: [Trace Listeners](https://docs.microsoft.com/en-us/dotnet/framework/debug-trace-profile/trace-listeners)
