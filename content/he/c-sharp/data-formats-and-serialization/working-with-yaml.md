---
title:                "עבודה עם YAML"
aliases: - /he/c-sharp/working-with-yaml.md
date:                  2024-02-03T19:25:14.099245-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
YAML, שפירושו "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא פורמט סידורי נתונים קריא לאדם. מתכנתים לעיתים קרובות משתמשים בו לקבצי תצורה, הודעות בין תהליכים, ואחסון נתונים בזכות פשטותו וקריאותו בהשוואה לפורמטים אחרים של נתונים כמו XML או JSON.

## איך לעשות:
C# אינו תומך באופן מובנה ב-YAML, אך ניתן לעבוד עם YAML בקלות באמצעות ספריות צד שלישי כמו *YamlDotNet*. ראשית, יש להתקין את החבילה YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### קריאת YAML:
דמיינו שיש לכם קובץ YAML `config.yaml` עם התוכן הבא:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

ניתן לקרוא ולנתח את הקובץ YAML הזה ב-C# כך:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // תאימו את קונבנציית השמות בהתאם
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**פלט לדוגמה:**
```
Name: MyApp, Version: 1.0.0
```

### כתיבת YAML:
כדי לכתוב נתונים לקובץ YAML, השתמשו במחלקת `Serializer` מ-YamlDotNet. הנה איך לדסריאליזציה של אובייקט חזרה ל-YAML:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // תאימו את קונבנציית השמות בהתאם
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**פלט לדוגמה:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

הגישה הישירה הזו מדגימה איך לעבוד ביעילות עם YAML בפרויקטים שלכם ב-C#, והופכת את הקריאה מ- והכתיבה לקבצי YAML באמצעות הספריה YamlDotNet לפשוטה.
