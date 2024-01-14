---
title:                "Kotlin: json과 함께 작업하기"
simple_title:         "json과 함께 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## 왜 프로그래머는 JSON을 다루는지

JSON은 현대의 소프트웨어 개발에서 빠질 수 없는 중요한 포맷입니다. 웹, 모바일 및 클라우드 애플리케이션에서 데이터를 전달하는 데 널리 사용되며, 출처와 형식에 독립적이기 때문에 매우 효율적입니다. 따라서 프로그래머라면 JSON을 이해하고 다룰 줄 알아야만 합니다.

## JSON 다루는 방법

```Kotlin
// JSON 데이터를 생성하는 예제
val json = jsonObject(
    "name" to "John",
    "age" to 35,
    "hobbies" to jsonArray("coding", "reading", "hiking")
)

// JSON 데이터를 읽는 예제
val name = json.getString("name") // 결과: John
val age = json.getInt("age") // 결과: 35
val hobbies = json.getJSONArray("hobbies") // 결과: ["coding", "reading", "hiking"]
```

## JSON에 대한 깊은 이해

JSON은 JavaScript Object Notation의 약자로, JavaScript와 동일한 기본 구문을 사용하지만 다른 언어에서도 쉽게 읽고 쓸 수 있도록 설계되었습니다. 보다 복잡한 구조의 데이터를 다룰 때 더 많은 기능을 사용할 수 있게 해주는 라이브러리도 있으니 참고하시기 바랍니다.

## 더 알아보기

[공식 Kotlin JSON 라이브러리 사용법](https://kotlinlang.org/docs/serialization-guides.html)

[코틀린을 이용한 간단한 JSON 다루기 예제](https://www.baeldung.com/kotlin-json)

[JSON 데이터를 다루는 다른 유용한 라이브러리들](https://www.thepolyglotdeveloper.com/2017/08/parsing-json-kotlin-android-using-gson/)