---
title:                "json과 함께 작업하기"
html_title:           "C#: json과 함께 작업하기"
simple_title:         "json과 함께 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 인터넷에서 데이터를 교환하기 위한 가장 일반적인 방법 중 하나입니다. 이를 사용하는 이유는 매우 다양하며, 대부분의 인터넷 서비스에서 JSON을 사용하므로 프로그래밍에서도 작동하는 것이 중요합니다.

## 방법

JSON을 사용하는 가장 간단한 방법은 C#에서 Newtonsoft.Json 패키지를 사용하는 것입니다. 이 패키지를 사용하려면, 다음과 같은 코드를 추가해야 합니다:

```C#
using Newtonsoft.Json;
```

이제 객체를 JSON 문자열로 변환하려면 다음과 같은 예제 코드를 사용할 수 있습니다:

```C#
var obj = new { Name = "John", Age = 25 };
var jsonString = JsonConvert.SerializeObject(obj);
Console.WriteLine(jsonString);
```

출력:

```console
{"Name":"John","Age":25}
```

반대로, JSON 문자열을 C# 객체로 변환하려면 다음과 같은 예제 코드를 사용할 수 있습니다:

```C#
string jsonString = "{\"Name\":\"John\",\"Age\":25}";
var obj = JsonConvert.DeserializeObject<dynamic>(jsonString);
Console.WriteLine(obj.Name);
Console.WriteLine(obj.Age);
```

출력:

```console
John
25
```

## 깊이 들어가기

JSON은 프로그래밍에서 사용하기 매우 유용한 데이터 형식입니다. 그러나 중첩된 객체나 배열과 같은 복잡한 구조를 다루기 위해서는 추가적인 공부가 필요합니다. 그래도 C#에서는 JObject나 JArray와 같은 클래스를 사용하여 복잡한 JSON 데이터를 다룰 수 있습니다. 또한 LINQ를 사용하여 JSON 질의도 가능합니다.

더 많은 정보를 알고 싶다면, 공식 문서나 인터넷에서 제공되는 다양한 자료를 참고하시기 바랍니다.

## 참고자료

- [Newtonsoft.Json 패키지](https://www.nuget.org/packages/Newtonsoft.Json/)
- [JSON 공식 문서](https://www.json.org/json-en.html)
- [LINQ 공식 문서](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
- [C#에서 JSON 다루기에 대한 블로그 포스트](https://www.c-sharpcorner.com/blogs/handling-json-format-in-c-sharp1)