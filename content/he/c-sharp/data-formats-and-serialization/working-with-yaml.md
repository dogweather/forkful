---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:14.099245-07:00
description: "YAML, \u05E9\u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \"YAML Ain't Markup\
  \ Language\" (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF), \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05D9\u05D3\
  \u05D5\u05E8\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0\
  \ \u05DC\u05D0\u05D3\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\
  \u05D5\u05E8\u05D4, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:37.616063-07:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \"YAML Ain't Markup Language\"\
  \ (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  ), \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05D9\u05D3\u05D5\u05E8\
  \u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\
  \u05D3\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D5 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\
  \u05D4, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
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
