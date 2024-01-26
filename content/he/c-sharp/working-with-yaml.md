---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים קל לקריאה, נוח לכתיבת קונפיגורציות, אחסון נתונים ותיעוד. תכניתנים משתמשים בו כי זה פשוט, נקי וידידותי לאדם.

## איך לעשות:
כדי לעבוד עם YAML ב-C#, תצטרך להשתמש בחבילה כמו YamlDotNet. תחילה, התקן את החבילה דרך NuGet.

```C#
// התקנת תיקיות YamlDotNet
Install-Package YamlDotNet -Version X.Y.Z
```

הנה דוגמה פשוטה של עיבוד YAML לאובייקט ב-C#:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Person {
    public string Name { get; set; }
    public int Age { get; set; }
}

class Program {
    static void Main() {
        var yaml = @"
name: Yossi
age: 30
";
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Use underscore casing
            .Build();

        var person = deserializer.Deserialize<Person>(yaml);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
    }
}
```

קלט:
```yaml
name: Yossi
age: 30
```

פלט:
```
Name: Yossi, Age: 30
```

## נחפר עמוק יותר
YAML (YAML Ain't Markup Language) התפתח בשנת 2001 כאלטרנטיבה ל-XML. ברבות השנים, הוא הפך לפופולרי בקרב פיתוח תוכנה על ידי הפשטות ועקביות שלו. בניגוד ל-JSON, YAML תומך בהערות, דבר שהופך אותו לאידיאלי עבור קבצי קונפיגורציה. חלופות אחרות כוללות TOML ו-JSON.

ב-C#, עליך להשתמש בביבליות צד שלישי כמו YamlDotNet, כיוון שלא קיימת תמיכה רשמית ב-YAML מ-Microsoft. כשעובדים עם YAML, חשוב לזכור את הכללים הבסיסיים להכתבה כמו רווחים, ענייני הזחה ומערכות האובייקטים.

## ראה גם
- תיעוד YamlDotNet: https://github.com/aaubry/YamlDotNet/wiki
- מורה ל-YAML: https://learnxinyminutes.com/docs/yaml/
- איך לכתוב מבנים מורכבים ב-YAML: https://en.wikipedia.org/wiki/YAML#Advanced_components
