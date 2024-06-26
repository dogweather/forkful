---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:19.765966-07:00
description: "\uBC29\uBC95: C#\uC740 \uD6A8\uC728\uC801\uC778 JSON \uCC98\uB9AC\uB97C\
  \ \uC704\uD574 `System.Text.Json` \uB124\uC784\uC2A4\uD398\uC774\uC2A4\uB97C \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. JSON \uBB38\uC790\uC5F4\uC744 C# \uAC1D\uCCB4\uB85C \uD30C\
  \uC2F1\uD558\uB824\uBA74 JSON \uAD6C\uC870\uC640 \uC77C\uCE58\uD558\uB294 \uD074\
  \uB798\uC2A4\uB97C \uC815\uC758\uD558\uACE0 `JsonSerializer.Deserialize` \uBA54\uC11C\
  \uB4DC\uB97C \uC0AC\uC6A9\uD558\uC2ED\uC2DC\uC624."
lastmod: '2024-03-13T22:44:55.266076-06:00'
model: gpt-4-0125-preview
summary: "C#\uC740 \uD6A8\uC728\uC801\uC778 JSON \uCC98\uB9AC\uB97C \uC704\uD574 `System.Text.Json`\
  \ \uB124\uC784\uC2A4\uD398\uC774\uC2A4\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
