---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:19.765966-07:00
description: "JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740\
  \ JSON \uB370\uC774\uD130\uB97C \uD30C\uC2F1, \uC0DD\uC131 \uBC0F \uCFFC\uB9AC\uD558\
  \uB294 \uAC83\uC744 \uD3EC\uD568\uD558\uC5EC \uD604\uB300 \uD504\uB85C\uADF8\uB798\
  \uBC0D\uC5D0 \uC788\uC5B4 \uD544\uC218\uC801\uC778 \uAE30\uC220\uC785\uB2C8\uB2E4\
  . \uC774 \uB370\uC774\uD130 \uAD50\uD658 \uD615\uC2DD\uC740 \uC77D\uAE30 \uC27D\uACE0\
  \ \uC5B8\uC5B4 \uB3C5\uB9BD\uC131\uC774 \uC788\uC5B4 \uC6F9 \uC11C\uBE44\uC2A4 \uBC0F\
  \ API\uC5D0\uC11C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB418\uACE0\u2026"
lastmod: '2024-03-13T22:44:55.266076-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 JSON\
  \ \uB370\uC774\uD130\uB97C \uD30C\uC2F1, \uC0DD\uC131 \uBC0F \uCFFC\uB9AC\uD558\uB294\
  \ \uAC83\uC744 \uD3EC\uD568\uD558\uC5EC \uD604\uB300 \uD504\uB85C\uADF8\uB798\uBC0D\
  \uC5D0 \uC788\uC5B4 \uD544\uC218\uC801\uC778 \uAE30\uC220\uC785\uB2C8\uB2E4. \uC774\
  \ \uB370\uC774\uD130 \uAD50\uD658 \uD615\uC2DD\uC740 \uC77D\uAE30 \uC27D\uACE0 \uC5B8\
  \uC5B4 \uB3C5\uB9BD\uC131\uC774 \uC788\uC5B4 \uC6F9 \uC11C\uBE44\uC2A4 \uBC0F API\uC5D0\
  \uC11C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB418\uACE0\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 무엇 & 왜?

JSON(JavaScript Object Notation)을 다루는 것은 JSON 데이터를 파싱, 생성 및 쿼리하는 것을 포함하여 현대 프로그래밍에 있어 필수적인 기술입니다. 이 데이터 교환 형식은 읽기 쉽고 언어 독립성이 있어 웹 서비스 및 API에서 광범위하게 사용되고 있어, 네트워크 응용 프로그램을 작업하거나 웹 기반 데이터와 상호 작용하는 C# 프로그래머에게 필수적입니다.

## 방법:

### JSON 문자열을 객체로 파싱하기

C#은 효율적인 JSON 처리를 위해 `System.Text.Json` 네임스페이스를 제공합니다. JSON 문자열을 C# 객체로 파싱하려면 JSON 구조와 일치하는 클래스를 정의하고 `JsonSerializer.Deserialize` 메서드를 사용하십시오.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 출력: Name: John, Age: 30
    }
}
```

### 객체에서 JSON 생성하기

C# 객체를 다시 JSON 문자열로 변환하려면 `JsonSerializer.Serialize` 메서드를 사용하십시오.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // 출력: {"Name":"Jane","Age":25}
    }
}
```

### Newtonsoft.Json 사용하기

`Newtonsoft.Json`(또는 Json.NET)은 JSON 직렬화 및 역직렬화를 위한 더 많은 유연성과 옵션을 제공하는 인기 있는 타사 라이브러리입니다.

Json.NET을 사용하려면 먼저 NuGet을 통해 `Newtonsoft.Json` 패키지를 설치해야 합니다. 그런 다음 다음과 같이 JSON 문자열을 역직렬화할 수 있습니다:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // 출력: Name: Mike, Age: 22
    }
}
```

Json.NET으로 객체에서 JSON 생성하기:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // 출력: {"Name":"Ella","Age":28}
    }
}
```

이 코드 조각들은 C#에서 JSON을 처리하는 방법에 대한 빠른 시작을 제공하며, 내장된 `System.Text.Json` 기능과 `Newtonsoft.Json`의 광범위한 기능을 모두 보여줍니다.
