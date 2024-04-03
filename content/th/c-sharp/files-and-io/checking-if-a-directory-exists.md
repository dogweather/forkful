---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:01.314579-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.240645-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:


### การใช้ System.IO
C# มี namespace `System.IO` ซึ่งมีคลาส `Directory` ที่เสนอวิธีที่ตรงไปตรงมาในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ผ่านวิธีการ `Exists`

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // ตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่
        bool directoryExists = Directory.Exists(directoryPath);

        // ปริ้นผลลัพธ์
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**ผลลัพธ์ตัวอย่าง:**

```
Directory exists: False
```

ในกรณีที่ไดเรกทอรีมีอยู่จริงที่เส้นทาง `C:\ExampleDirectory` ผลลัพธ์จะเป็น `True`

### การใช้ System.IO.Abstractions สำหรับการทดสอบหน่วย
เมื่อคุณต้องการทำให้โค้ดของคุณสามารถทดสอบหน่วย โดยเฉพาะเมื่อมีการโต้ตอบกับระบบไฟล์ แพคเกจ `System.IO.Abstractions` เป็นทางเลือกที่นิยม มันอนุญาตให้คุณสามารถแยกและจำลองการทำงานของระบบไฟล์ในการทดสอบของคุณ นี่คือวิธีที่คุณสามารถตรวจสอบว่ามีไดเรกทอรีอยู่โดยใช้วิธีนี้:

ก่อนอื่น ตรวจสอบว่าคุณได้ติดตั้งแพคเกจ:

```
Install-Package System.IO.Abstractions
```

จากนั้น คุณสามารถฉีด `IFileSystem` เข้าไปในคลาสของคุณและใช้มันเพื่อตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ ซึ่งทำให้การทดสอบหน่วยง่ายขึ้น

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**ผลลัพธ์ตัวอย่าง:**

```
Directory exists: False
```

วิธีการนี้ทำให้โลจิกของแอปพลิเคชันของคุณถูกแยกออกจากการเข้าถึงระบบไฟล์โดยตรง ทำให้โค้ดของคุณมีโครงสร้างที่ดีขึ้น สามารถทดสอบได้ และรักษาได้ง่ายขึ้น
