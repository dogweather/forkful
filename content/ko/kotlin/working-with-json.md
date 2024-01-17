---
title:                "Json으로 작업하기"
html_title:           "Kotlin: Json으로 작업하기"
simple_title:         "Json으로 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# JSON 작업하기와 그 이유
JSON(JavaScript Object Notation)은 자바스크립트에서 데이터를 교환하기 위해 널리 사용되는 형식입니다. 프로그래머들은 JSON을 사용하여 서로 다른 시스템 간에 데이터를 전송하고, 저장하고, 읽을 수 있습니다.

# 사용 방법:
- JSON 파일 작성하기:
```
Kotlin val json = """{ 
    "name": "John", 
    "age": 30, 
    "city": "Seoul"
}"""
```
- JSON 파싱하기:
```
Kotlin val name = json.getString("name") 
val age = Jason.getInt("age") 
val city = Jason.getString("city")
```
- 코드에서 JSON 사용하기:
```
Kotlin val obj = JsonObject() 
obj.addProperty("name", "Jane") 
obj.addProperty("age", 25) 
val jsonStr = obj.toString()
```
- 샘플 출력:
```
{
    "name": "Jane", 
    "age": 25
}
```

# 딥 다이브:
- 역사적 맥락: JSON은 2002년 도우온 크록포드에 의해 최초로 개발되었습니다. 원래는 자바스크립트에서 사용하기 위해 만들어졌지만, 이제는 많은 언어에서 지원되고 있습니다.
- 대안: XML과 같은 다른 데이터 형식도 있지만, JSON은 구문이 간단하고 더 가벼우며, 널리 사용되기 때문에 더 인기가 있습니다.
- 구현 세부사항: Kotlin에서는 내장된 JSONObject 및 JSONArray 클래스를 사용하여 JSON 데이터를 생성하고 해석할 수 있습니다.

# 관련 자료:
- [JSON 공식 사이트](https://www.json.org/)
- [Kotlin에서 JSON 다루기](https://www.raywenderlich.com/367-kotlin-json-tutorial-getting-started)