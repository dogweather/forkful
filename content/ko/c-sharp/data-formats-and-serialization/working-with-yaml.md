---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:03.958619-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uADF8 \uAC04\uB2E8\uD568\uACFC \uAC00\uB3C5\
  \uC131 \uB54C\uBB38\uC5D0 \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815\
  \ \uD30C\uC77C, \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\uC9C0 \uC804\uC1A1\
  , \uB370\uC774\uD130 \uC800\uC7A5 \uB4F1\uC5D0 XML\uC774\uB098 JSON \uAC19\uC740\
  \ \uB2E4\uB978 \uB370\uC774\uD130 \uD615\uC2DD\uBCF4\uB2E4 YAML\uC744\u2026"
lastmod: '2024-02-25T18:49:52.259496-07:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uC0AC\
  \uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654\
  \ \uD615\uC2DD\uC785\uB2C8\uB2E4. \uADF8 \uAC04\uB2E8\uD568\uACFC \uAC00\uB3C5\uC131\
  \ \uB54C\uBB38\uC5D0 \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815 \uD30C\
  \uC77C, \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\uC9C0 \uC804\uC1A1, \uB370\uC774\
  \uD130 \uC800\uC7A5 \uB4F1\uC5D0 XML\uC774\uB098 JSON \uAC19\uC740 \uB2E4\uB978\
  \ \uB370\uC774\uD130 \uD615\uC2DD\uBCF4\uB2E4 YAML\uC744\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML은 "YAML Ain't Markup Language"의 약자로, 사람이 읽을 수 있는 데이터 직렬화 형식입니다. 그 간단함과 가독성 때문에 프로그래머들은 설정 파일, 프로세스 간 메시지 전송, 데이터 저장 등에 XML이나 JSON 같은 다른 데이터 형식보다 YAML을 종종 사용합니다.

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
