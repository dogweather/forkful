---
title:                "Java: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜 YAML을 사용해야 할까요?

YAML은 데이터 serialization과 configuration 파일 형식으로 널리 사용되는 경량 마크업 언어입니다. Java 프로그래밍에서 YAML을 이용하면 데이터를 쉽게 읽고 쓸 수 있으며, 코드를 단순하게 유지할 수 있습니다.

## 어떻게 YAML을 사용할까요?

YAML을 사용하기 위해서는 먼저 YAML 파일을 읽어들일 수 있는 라이브러리가 필요합니다. 그리고 YAML 파일의 내용을 파싱하여 객체로 매핑해주는 작업이 필요합니다. 예를 들어, 아래의 YAML 파일을 Java로 읽어들이고 매핑하는 예제를 살펴보겠습니다.

```Java
// YAML 파일 읽어들이기
InputStream inputStream = new FileInputStream("config.yaml");
// YAML 파일의 내용을 매핑
Yaml yaml = new Yaml();
Map<String, Object> data = yaml.load(inputStream);
// 매핑된 데이터 출력
System.out.println(data);
```

위의 코드를 실행하면 아래와 같은 출력이 나타납니다.

```Java
{
  name: John Smith,
  age: 25,
  hobbies: [reading, hiking, cooking]
}
```

YAML 파일에 있는 데이터를 매핑하여 Java에서 사용하기 편리하게 만들 수 있습니다.

## YAML의 깊은 곳으로

더 많은 정보를 알고 싶다면, YAML의 [공식 사이트](https://yaml.org/)를 방문하실 수 있습니다. 그리고 YAML을 사용하는 다양한 방법과 라이브러리에 대해 알아보려면 [YAML.org 사용자 지침서](https://yaml.org/spec/1.2/spec.html)를 참고하시기 바랍니다.

# 더 알아보기

- [Java 코드에서 YAML 파일 읽기](https://www.baeldung.com/java-snake-yaml)
- [Spring Boot에서 YAML 파일 사용하기](https://www.baeldung.com/spring-yaml)