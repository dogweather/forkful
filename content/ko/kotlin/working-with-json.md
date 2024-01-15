---
title:                "JSON 작업하기"
html_title:           "Kotlin: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현대의 개발에서 매우 중요한 역할을 합니다. 거의 모든 애플리케이션에서 데이터를 송수신하고 저장하기 위해 JSON을 사용하고 있습니다. JSON은 가볍고 읽기 쉬우며, 여러 형식의 데이터를 효율적으로 다루는 데 도움이 됩니다.

## 사용 방법

JSON을 다루는 가장 간단하고 간결한 방법은 Kotlin의 ```JSONObject```와 ```JSONArray``` 클래스를 사용하는 것입니다. 먼저 JSON을 가져오기 위해 표준 라이브러리에서 제공하는 ```JSONObject``` 객체를 사용합니다. 그러면 데이터를 쉽게 다룰 수 있습니다. 예를 들어, JSON 문자열을 파싱하여 사용자의 이름을 가져올 수 있습니다.

```Kotlin
val jsonString = "{\"name\": \"John\", \"age\": 32}"
val user = JSONObject(jsonString)
val name = user.getString("name")
println(name) // John
```

```JSONObject```를 수정하고 다른 데이터를 추가하려면, ```put()``` 메서드를 사용합니다.

```Kotlin
user.put("occupation", "Developer")
val updatedJsonString = user.toString()
println(updatedJsonString) // {"name": "John", "age": 32, "occupation": "Developer"}
```

배열 형식의 데이터를 다루기 위해 ```JSONArray``` 클래스를 사용할 수 있습니다. 예를 들어, 여러 사용자의 정보를 담은 JSON 배열을 파싱하고 반복문을 통해 사용자 이름을 출력할 수 있습니다.

```Kotlin 
val jsonArray = "[{\"name\": \"John\", \"age\": 32}, {\"name\": \"Emily\", \"age\": 28}]"
val users = JSONArray(jsonArray)

for (i in 0 until users.length()) {
    val name = users.getJSONObject(i).getString("name")
    println(name) // John, Emily
}
```

## 딥 다이브

더욱 복잡한 JSON 데이터를 다루기 위해선, 스크립트를 작성하는 데 도움이 되는 외부 라이브러리를 사용할 수 있습니다. 여러 가지 라이브러리 중에서도 유명한 라이브러리 중 하나인 ```Gson```을 살펴보겠습니다. 이 라이브러리는 JSON 데이터를 Java 또는 Kotlin 객체로 변환하는 데 유용하며, Gson을 사용하면 데이터 및 객체 간의 변환 과정을 더욱 쉽게할 수 있습니다.

먼저, Gradle 또는 Maven을 통해 Gson을 프로젝트에 추가합니다. 그런 다음 Gson을 사용하여 JSON 문자열을 Java 또는 Kotlin 객체로 변환할 수 있습니다. Gson은 이를 위해 어노테이션을 사용하므로, 변환할 데이터 클래스를 구성할 때 어노테이션을 추가해야 합니다. 따라서 Gson을 사용하여 JSON 데이터를 변환할 때 더 쉽고 깔끔한 코드를 작성할 수 있습니다.

## 더보기

- JSON 개념 이해: https://medium.com/@davidnajjar/json-%EA%B0%9C%EB%85%90-%EB%B0%8F-%ED%98%84%EB%8C%80%ED%95%9C-%EC%9E%91%EB%8F%99-%EC%95%8C%EA%B3%A0%EB%A6%AC%EC%A6%98-795d3e1e7e70
- Kotlin에서 외부 라이브러리 사용하기: https://www.baeldung.com/kotlin-external-java-libraries
- Gson 공식 문서: https://github.com/google/gson