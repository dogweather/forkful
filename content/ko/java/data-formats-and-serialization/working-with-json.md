---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:40.314975-07:00
description: "JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\
  \uC740 \uC790\uBC14 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\uC5D0\uC11C \uC774\
  \ \uACBD\uB7C9 \uB370\uC774\uD130 \uAD50\uD658 \uD3EC\uB9F7\uC744 \uCC98\uB9AC\uD55C\
  \uB2E4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\uB97C \uB124\uD2B8\
  \uC6CC\uD06C\uB97C \uD1B5\uD574 \uC9C1\uB82C\uD654\uD558\uACE0 \uC804\uC1A1\uD558\
  \uBA70, \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD6C\uC131\uD558\uACE0 \uC800\uC7A5\
  \uD560 \uC218 \uC788\uB3C4\uB85D \uC778\uAC04\uC774\u2026"
lastmod: '2024-03-13T22:44:55.081030-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\uC740\
  \ \uC790\uBC14 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\uC5D0\uC11C \uC774 \uACBD\
  \uB7C9 \uB370\uC774\uD130 \uAD50\uD658 \uD3EC\uB9F7\uC744 \uCC98\uB9AC\uD55C\uB2E4\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
