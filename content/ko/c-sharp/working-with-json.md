---
title:                "C#: JSON과 함께 작업하기"
simple_title:         "JSON과 함께 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON 작업을 해야하는가?

JSON은 현대 프로그래밍에서 매우 중요한 역할을 합니다. 데이터 교환과 데이터 저장에 사용되기 때문에 많은 개발자들이 이를 다루어야 합니다. C# 프로그래밍에서도 JSON을 다루는 방법을 배우는 것이 매우 중요합니다. 여기에서는 JSON을 다루는 기초적인 방법을 소개하겠습니다.

## 어떻게 하면 될까요?

JSON을 사용하기 위해서는 먼저 Newtonsoft.Json 라이브러리를 사용해야 합니다. 이 라이브러리는 JSON 파싱과 생성에 매우 편리한 기능을 제공합니다. 또한 C#의 기본 클래스들도 JSON 데이터를 다룰 수 있습니다. 아래는 JsonReader를 사용하여 JSON 파일을 읽고, JObject를 사용하여 데이터를 파싱하는 예시 코드입니다.

```C#
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

// JSON 파일 읽기
using (StreamReader file = File.OpenText("example.json"))
{
    string jsonString = file.ReadToEnd(); // 문자열로 전체 JSON 파일을 읽어옴
    
    // JSON 파싱
    using (JsonReader reader = new JsonTextReader(new StringReader(jsonString)))
    {
        JObject json = (JObject)JToken.ReadFrom(reader);
        
        // 객체에 접근하여 데이터 추출
        string name = json["name"].Value<string>();
        int age = json["age"].Value<int>();
        
        // 배열에 접근하여 데이터 추출
        JArray hobbies = json["hobbies"].Value<JArray>();
        foreach (JValue hobby in hobbies)
        {
            Console.WriteLine(hobby.Value<string>());
        }
    }
}
```

위 코드를 실행하면 다음과 같은 출력이 나타납니다.

```
John
24
Soccer
Reading
```

C#에는 위와 같은 기본 클래스 이외에도 다양한 방법으로 JSON 데이터를 다룰 수 있습니다. 이는 각자의 프로젝트에 맞게 선택하여 사용할 수 있습니다.

## 깊게 들어가기

JSON 데이터를 다루기 위해서는 데이터 형식과 데이터 구조에 대한 이해가 중요합니다. 예를 들어, 자동차의 정보를 담고 있는 JSON 데이터가 있다고 할 때, "브랜드"와 "차종"이라는 정보는 어떻게 구조화되어 저장될까요? 또한 배열 형식으로 데이터를 저장할 때 각 객체의 특정 속성이 어떻게 지정되는지도 알아야 합니다. JSON 데이터를 다룰 때 이러한 기본적인 지식은 필수적입니다.

더 자세한 정보는 다음의 링크를 참고하세요.

## 관련 링크

- [Programming with JSON in C#](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [JSON Data Types](https://www.json.org/json-en.html)
- [JSON Array](https://www.w3schools.com/js/js_arrays.asp)