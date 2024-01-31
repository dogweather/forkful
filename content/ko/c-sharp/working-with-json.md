---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 쓰나요?)
JSON은 데이터 교환 포맷입니다. 간결하고, 읽기 쉬우며, 대부분의 프로그래밍 언어에 구현이 있습니다. 개발자는 구조화된 데이터를 서버와 클라이언트 간에 주고받기 위해 JSON을 사용합니다.

## How to: (어떻게 하죠?)
C#에서 JSON 다루는 방법을 간단히 보여드리겠습니다.

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // JSON 문자열을 C# 객체로 변환
        string jsonString = "{\"firstName\":\"Jihun\",\"lastName\":\"Kim\"}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);
        Console.WriteLine($"이름: {person.FirstName}, 성: {person.LastName}");

        // C# 객체를 JSON 문자열로 변환
        Person newPerson = new Person { FirstName = "Soyeon", LastName = "Lee" };
        string newJsonString = JsonSerializer.Serialize(newPerson);
        Console.WriteLine(newJsonString);
    }
}

public class Person
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
}
```

출력값:

```
이름: Jihun, 성: Kim
{"firstName":"Soyeon","lastName":"Lee"}
```

## Deep Dive (더 알아보기)
JSON은 JavaScript Object Notation의 약자로 2001년에 Douglas Crockford에 의해 고안되었습니다. XML이나 YAML 같은 다른 데이터 포맷들도 있지만, JSON은 가벼움과 간결함 때문에 선호됩니다. C#에서 System.Text.Json 라이브러리는 높은 성능과 저 메모리 사용량으로 유명합니다. .NET Core 3.0 이상부터 기본 제공되며, Newtonsoft.Json 같은 타 라이브러리와 비교할 때 확장성과 속도면에서 강점을 보입니다.

## See Also (참고할 링크)
- [JSON 공식 웹사이트](https://www.json.org/json-en.html)
- [.NET의 System.Text.Json 도움말](https://docs.microsoft.com/en-us/dotnet/api/system.text.json)
- [Newtownsoft.Json 라이브러리](https://www.newtonsoft.com/json)
