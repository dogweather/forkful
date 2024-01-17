---
title:                "JSON 처리하기"
html_title:           "C#: JSON 처리하기"
simple_title:         "JSON 처리하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON 처리는 데이터 교환과 저장에 널리 사용되는 간단한 형식입니다. 프로그래머들은 JSON을 사용하여 데이터를 쉽게 읽고 쓸 수 있고, 다른 시스템과 데이터를 공유하기에도 편리합니다.

## 방법:

JSON을 처리하는 가장 간단한 방법은 Newtonsoft의 JSON.NET 라이브러리를 사용하는 것입니다. 다음과 같이 간단한 예제를 통해 어떻게 사용하는지 살펴보겠습니다:

```C#
// Serialize to JSON
var person = new Person
{
    Name = "John",
    Age = 30
};
var json = JsonConvert.SerializeObject(person);

// Output: {"Name":"John","Age":30}

// Deserialize from JSON
var json = @"{'Name':'Jane','Age':25}";
var person = JsonConvert.DeserializeObject<Person>(json);

// Output:
// Name: Jane
// Age: 25
```

## 깊이 파보기:

JSON은 1999년 더그 커락(Douglas Crockford)에 의해 만들어졌으며, 현재는 데이터 교환과 웹 애플리케이션에서 매우 널리 사용되고 있습니다. 다른 대안으로는 XML, YAML, CSV 등이 있지만, JSON은 구조가 간단하고 가벼워서 널리 사용됩니다.

JSON 데이터를 처리하는 데에는 많은 라이브러리가 있지만, JSON.NET은 가장 널리 사용되는 라이브러리입니다. 이 라이브러리는 높은 성능과 다양한 기능으로 인기를 끌고 있습니다.

## 참고 자료:

- [JSON.NET 공식 사이트](https://www.newtonsoft.com/json)
- [JSON 표준 문서](https://www.json.org/json-ko.html)