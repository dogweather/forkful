---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML은 데이터 직렬화 포맷 중 하나로, 설정 파일이나 데이터 교환에 쓰입니다. 프로그래머는 가독성이 높고 휴먼 에러에 강하기 때문에 YAML을 많이 사용합니다.

## How to:

C# 프로그래머들은 YAML 다루기 위해 `YamlDotNet` 라이브러리를 자주 사용합니다. NuGet 패키지 매니저로 설치하고 예제 코드로 배워보세요.

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var yaml = @"
name: Kim
age: 30
language: C#
";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        var person = deserializer.Deserialize<Person>(yaml);
        
        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}, Language: {person.Language}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Language { get; set; }
}
```

출력:
```
Name: Kim, Age: 30, Language: C#
```

## Deep Dive

YAML은 "YAML Ain't Markup Language"(원래 "Yet Another Markup Language")의 준말이며 2001년에 개발되었습니다. JSON과 같은 다른 데이터 포맷을 대체할 수 있지만, 주석 사용 가능과 계층 구조 표현이 더 선명하다는 장점이 있습니다. `YamlDotNet` 라이브러리는 .NET 환경에서 YAML 직렬화 및 역직렬화를 수행하는 구현체입니다.

## See Also

- YamlDotNet GitHub 리포지토리: [https://github.com/aaubry/YamlDotNet](https://github.com/aaubry/YamlDotNet)
- YAML 공식 웹사이트: [https://yaml.org/](https://yaml.org/)
