---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:03.958619-07:00
description: "\uBC29\uBC95: C#\uC740 YAML\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\
  \uC774 \uC5C6\uC9C0\uB9CC, *YamlDotNet*\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC27D\uAC8C YAML\uC744\
  \ \uB2E4\uB8F0 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800 YamlDotNet \uD328\uD0A4\
  \uC9C0\uB97C \uC124\uCE58\uD574\uC57C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.264544-06:00'
model: gpt-4-0125-preview
summary: "C#\uC740 YAML\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uC9C0\
  \uB9CC, *YamlDotNet*\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC27D\uAC8C YAML\uC744 \uB2E4\uB8F0\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
C#은 YAML에 대한 내장 지원이 없지만, *YamlDotNet*과 같은 서드파티 라이브러리를 사용하여 쉽게 YAML을 다룰 수 있습니다. 먼저 YamlDotNet 패키지를 설치해야 합니다:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### YAML 읽기:
다음 내용이 포함된 YAML 파일 `config.yaml`을 가지고 있다고 가정해 봅시다:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

다음과 같이 C#에서 이 YAML 파일을 읽고 파싱할 수 있습니다:
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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 네이밍 컨벤션을 적절히 조정
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"이름: {config.appSettings.name}, 버전: {config.appSettings.version}");
    }
}
```
**샘플 출력:**
```
이름: MyApp, 버전: 1.0.0
```

### YAML 쓰기:
YAML 파일에 데이터를 작성하려면 YamlDotNet의 `Serializer` 클래스를 사용하세요. 객체를 다시 YAML로 직렬화하는 방법은 다음과 같습니다:

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
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // 네이밍 컨벤션을 적절히 조정
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**샘플 출력:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

이 간단한 방법은 C# 프로젝트에서 YAML을 효과적으로 다루는 방법을 보여줍니다. YamlDotNet 라이브러리를 사용하여 YAML 파일을 읽고 쓰기가 간단하다는 것을 알 수 있습니다.
