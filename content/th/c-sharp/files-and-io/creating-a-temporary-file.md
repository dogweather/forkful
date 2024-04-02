---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:16.359856-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0A\u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\u0E30\u0E2A\u0E07\
  \u0E04\u0E4C\u0E23\u0E30\u0E22\u0E30\u0E2A\u0E31\u0E49\u0E19, \u0E40\u0E0A\u0E48\
  \u0E19 \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E35\u0E22\
  \u0E07\u0E43\u0E19\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u2026"
lastmod: '2024-03-17T21:57:56.245472-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0A\u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\u0E30\u0E2A\u0E07\
  \u0E04\u0E4C\u0E23\u0E30\u0E22\u0E30\u0E2A\u0E31\u0E49\u0E19, \u0E40\u0E0A\u0E48\
  \u0E19 \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E35\u0E22\
  \u0E07\u0E43\u0E19\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u2026"
title: "\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\u0E48\u0E27\
  \u0E04\u0E23\u0E32\u0E27"
weight: 21
---

## คืออะไร & ทำไม?
การสร้างไฟล์ชั่วคราวหมายถึงการสร้างไฟล์เพื่อวัตถุประสงค์ระยะสั้น, เช่น การเก็บข้อมูลที่ต้องการเพียงในระหว่างการทำงานของโปรแกรม นักพัฒนาทำการนี้ด้วยเหตุผลต่างๆ เช่น เพื่อหลีกเลี่ยงข้อจำกัดด้านหน่วยความจำหรือเก็บสถานะชั่วคราวระหว่างการดำเนินงานที่ซับซ้อน

## วิธีการ:
นี่คือวิธีที่รวดเร็วในการสร้างและเขียนไฟล์ชั่วคราวใน C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // สร้างไฟล์ชั่วคราว
        string tempFilePath = Path.GetTempFileName();

        // เขียนบางอย่างลงไฟล์ชั่วคราว
        File.WriteAllText(tempFilePath, "Hello, Temp World!");

        // อ่านคืนจากไฟล์ชั่วคราวและพิมพ์
        string fileContents = File.ReadAllText(tempFilePath);
        Console.WriteLine(fileContents);

        // ทำความสะอาดไฟล์ชั่วคราว
        File.Delete(tempFilePath);
    }
}
```

ตัวอย่างผลลัพธ์:
```
Hello, Temp World!
```

## วิเคราะห์อย่างละเอียด
ไฟล์ชั่วคราวได้มีอยู่มานานตั้งแต่ยุคแรกๆ ของการคำนวณเมื่อการลดการใช้งานหน่วยความจำเป็นสิ่งสำคัญ พวกเขาให้สภาพแวดล้อมที่แยกออกสำหรับโปรแกรมในการทำงานกับข้อมูลโดยไม่มีผลกระทบการเก็บข้อมูลระยะยาว

นอกเหนือจาก `Path.GetTempFileName()`, คุณมีตัวเลือกอื่นๆ เช่น `Path.GetRandomFileName()`, ไม่สร้างไฟล์ แต่ให้ชื่อที่คุณสามารถใช้สำหรับไฟล์ชั่วคราว นอกจากนี้, คลาส `System.IO.TempFileCollection` สามารถจัดการไฟล์ชั่วคราวหลายไฟล์, ซึ่งมีประโยชน์เมื่อคุณต้องการมากกว่าหนึ่งไฟล์

ใต้ฮู้ด, วิธีการส่วนใหญ่ใน C# สำหรับการสร้างไฟล์ชั่วคราวใช้ API ที่ระบบปฏิบัติการชั้นใต้ให้มา ใน Windows, `GetTempFileName()` มีการแมปไปยังฟังก์ชัน API Win32 ที่คล้ายกันซึ่งรับประกันความเป็นเอกลักษณ์ของชื่อไฟล์ และรักษาความปลอดภัยจากการชนกัน

จำเป็นต้องลบไฟล์ชั่วคราวเสมอ แม้ว่าจะอยู่ในไดเร็คทอรีชั่วคราว แต่พวกเขาสามารถสะสมขึ้นถ้าถูกละเลย เป็นประเภทของฝันร้ายการกักตุนข้อมูลดิจิทัล

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติมและการเข้าใจอย่างลึกซึ้ง, ลิงก์เหล่านี้ควรครอบคลุมทุกสิ่งที่คุณต้องการ:

- เอกสารอย่างเป็นทางการของไมโครซอฟท์เกี่ยวกับไฟล์ชั่วคราวใน .NET:
  [ไฟล์ชั่วคราวใน .NET](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-temporary-file)

- ข้อปฏิบัติที่ดีที่สุดสำหรับการทำงานกับไฟล์และสตรีมใน C#:
  [File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io)

- ถ้าคุณต้องการสำรวจการพิจารณาความปลอดภัยของไฟล์ I/O:
  [File I/O and Security](https://docs.microsoft.com/en-us/dotnet/standard/security/secure-file-i-o)
