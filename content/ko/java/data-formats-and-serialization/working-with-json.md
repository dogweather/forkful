---
title:                "JSON과 함께 일하기"
aliases:
- /ko/java/working-with-json.md
date:                  2024-02-03T19:23:40.314975-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
JSON(JavaScript Object Notation)을 다룬다는 것은 자바 애플리케이션 내에서 이 경량 데이터 교환 포맷을 처리한다는 것을 의미합니다. 프로그래머들은 구조화된 데이터를 네트워크를 통해 직렬화하고 전송하며, 데이터를 쉽게 구성하고 저장할 수 있도록 인간이 읽을 수 있고 언어에 독립적인 JSON을 선택합니다.

## 방법:
자바에서 JSON으로 코딩하는 법을 알아봅시다.

첫 번째로, `Jackson` 또는 `Google Gson`과 같은 JSON 처리 라이브러리가 필요합니다. 여기에서는 `Jackson`을 사용할 것이므로 `pom.xml`에 이 의존성을 추가하세요:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

이제, 간단한 자바 객체를 JSON으로 직렬화(쓰기)합시다:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

출력 결과는 다음과 같습니다:

```json
{"name":"Alex","age":30}
```

이제, JSON을 다시 자바 객체로 역직렬화(읽기)합니다:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + "는 " + person.age + "세 입니다.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

출력 결과는 다음과 같을 겁니다:

```
Alex는 30세 입니다.
```

## 심화 학습
JSON의 단순함과 효율성은 웹에서 데이터 교환의 사실상의 표준으로 자리잡게 만들었으며, XML을 그 왕좌에서 물러나게 만들었습니다. 2000년대 초반에 도입된 JSON은 자바스크립트에서 파생되었지만 이제 대부분의 언어에서 지원됩니다.

JSON의 대안으로는 보다 장황한 XML과 Protocol Buffers 또는 MessagePack과 같은 이진 포맷이 있습니다. 이진 포맷들은 사람이 읽기 더 어렵지만 크기와 속도 면에서 더 효율적입니다. 각자의 사용 사례가 있으며, 선택은 특정 데이터 요구 사항과 상황에 달려 있습니다.

`Jackson`과 `Gson` 외에도 자바에서 JSON을 처리하기 위해 `JsonB`와 `org.json`과 같은 라이브러리가 있습니다. Jackson은 스트림 기반 처리를 제공하며 속도로 유명하고, Gson은 사용하기 쉽다는 것으로 유명합니다. JsonB는 보다 표준화된 접근법을 제공하는 Jakarta EE의 일부입니다.

JSON을 구현할 때, 잘못된 입력에 대해 코드가 강건해야 하므로 예외 처리를 제대로 다루는 것을 기억하세요. 또한 자동 데이터 바인딩의 보안 함의를 고려하세요 – 항상 입력을 검증하세요!

## 참고
- [Jackson 프로젝트](https://github.com/FasterXML/jackson)
- [Gson 프로젝트](https://github.com/google/gson)
- [JSON 사양](https://www.json.org/json-en.html)
- [JsonB 사양](https://jakarta.ee/specifications/jsonb/)
