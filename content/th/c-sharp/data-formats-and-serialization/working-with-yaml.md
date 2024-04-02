---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:15.369975-06:00
description: "YAML \u0E0B\u0E36\u0E48\u0E07\u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\
  \u0E01 YAML Ain't Markup Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\
  \u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\
  \u0E48\u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.246416-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E0B\u0E36\u0E48\u0E07\u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01\
  \ YAML Ain't Markup Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\
  \u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\
  \u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E21\
  \u0E19\u0E38\u0E29\u0E22\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\
  \u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## อะไร & ทำไม?
YAML ซึ่งย่อมาจาก YAML Ain't Markup Language เป็นรูปแบบการซีเรียลไลซ์ข้อมูลที่อ่านได้ง่ายสำหรับมนุษย์ โปรแกรมเมอร์มักใช้เพื่อไฟล์การกำหนดค่า, การส่งข้อความระหว่างกระบวนการ, และการเก็บข้อมูล เนื่องจากมีความเรียบง่ายและความสามารถในการอ่านได้ง่ายเมื่อเปรียบเทียบกับรูปแบบข้อมูลอื่น ๆ เช่น XML หรือ JSON

## วิธีการ:
C# ไม่มีการสนับสนุน YAML ในตัว แต่คุณสามารถทำงานกับ YAML ได้อย่างง่ายดายโดยใช้ไลบรารีของบุคคลที่สาม เช่น *YamlDotNet* ก่อนอื่นคุณต้องติดตั้งแพ็กเกจ YamlDotNet:
```bash
Install-Package YamlDotNet -Version 11.2.1
```

### การอ่าน YAML:
จินตนาการว่าคุณมีไฟล์ YAML `config.yaml` ที่มีเนื้อหาดังนี้:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

คุณสามารถอ่านและแยกวิเคราะห์ไฟล์ YAML นี้ใน C# ดังนี้:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // ปรับกฎการตั้งชื่อตามข้อกำหนด
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**ผลลัพธ์ตัวอย่าง:**
```
Name: MyApp, Version: 1.0.0
```

### การเขียน YAML:
ในการเขียนข้อมูลลงไฟล์ YAML ให้ใช้คลาส `Serializer` จาก YamlDotNet นี่คือวิธีการซีเรียลอ็อบเจ็คกลับไปยัง YAML:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // ปรับกฎการตั้งชื่อตามข้อกำหนด
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**ผลลัพธ์ตัวอย่าง:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

วิธีการนี้แสดงให้เห็นวิธีการทำงานกับ YAML ในโปรเจกต์ C# ของคุณอย่างมีประสิทธิภาพ เพื่อให้การอ่านและเขียนไฟล์ YAML ง่ายดายโดยใช้ไลบรารี YamlDotNet.
