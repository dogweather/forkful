---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
JSON은 데이터 교환 포맷입니다. 쉽고, 가볍기 때문에 많은 프로그래머들이 API 통신이나 설정 파일 등에 사용합니다.

## How to: (어떻게 사용하나요?)
Java에서 JSON 다루려면 외부 라이브러리 필요. 여기서는 `Gson` 사용법을 보여줌.

```java
import com.google.gson.Gson;

public class JsonExample {
    public static void main(String[] args) {
        // JSON 문자열로부터 객체 생성
        String json = "{\"name\":\"James\", \"age\":25}";
        Gson gson = new Gson();
        Person person = gson.fromJson(json, Person.class);
        System.out.println(person.getName() + "는 " + person.getAge() + "살입니다.");

        // 객체로부터 JSON 문자열 생성
        person.setAge(26);
        String newJson = gson.toJson(person);
        System.out.println(newJson);
    }
}

class Person {
    private String name;
    private int age;
    
    // getter와 setter 생략
}
```
출력:
```
James는 25살입니다.
{"name":"James","age":26}
```

## Deep Dive (심층 분석)
JSON(JavaScript Object Notation)은 데이터 형식을 나타내는 표준, 2001년에 douglas Crockford가 소개함. XML 대안으로 많이 사용. Java에서는 Gson, Jackson, JSON-B 등 여러 라이브러리로 JSON 처리 가능. 성능, 사용 편의성, 기능면에서 차이가 있음.

## See Also (추가 자료)
- Google Gson 공식 문서: https://github.com/google/gson
- Jackson 공식 사이트: https://github.com/FasterXML/jackson
- JSON-B 사양: https://javaee.github.io/jsonb-spec/
