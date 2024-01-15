---
title:                "yaml를 다루는 방법"
html_title:           "Gleam: yaml를 다루는 방법"
simple_title:         "yaml를 다루는 방법"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML을 사용해야 할까요?

YAML은 인간과 컴퓨터 모두가 쉽게 읽고 작성할 수 있는 형식의 데이터 직렬화 방식입니다. 따라서 YAML을 사용하면 더 간단하고 효율적인 데이터 관리가 가능해집니다.

## 사용 방법

```Gleam
fn main() {
  let data = gleam_yml::from_string("
    username: John
    age: 27
    hobbies:
        - hiking
        - reading
        - coding
  ")
  let username = data.username
  let age = data.age
  let hobbies = data.hobbies
  IO.println("Username: " ++ username)
  IO.println("Age: " ++ age)
  IO.println("Hobbies: " ++ hobbies)
}
```

위의 코드에서는 Gleam 패키지 중 하나인 gleam_yml을 사용하여 YAML 형식의 데이터를 읽고 출력하는 방법을 보여줍니다. 데이터를 읽을 때에는 간단한 키-값 쌍을 사용하고, 리스트 형태로 값을 전달할 수도 있습니다. 이를 이용하면 보다 복잡한 데이터도 쉽게 다룰 수 있습니다.

YAML 라이브러리에는 gleam_yml 이외에도 다양한 패키지가 있습니다. 필요에 따라 다른 라이브러리를 사용해보는 것도 좋은 방법일 수 있습니다.

## 깊이 들어가기

YAML은 공백과 들여쓰기를 사용하여 데이터 구조를 표현합니다. 따라서 간단한 키-값 쌍의 경우에는 YAML 문법에 대한 이해가 필요하지 않지만, 보다 복잡한 데이터 구조를 다루기 위해서는 YAML 문법을 익히는 것이 중요합니다.

또한, YAML은 표준 형식이 아닌 형식으로 기술되어 있기 때문에, 다른 언어나 툴과의 호환성에 대해서도 주의해야 합니다. 예를 들어, YAML 내에서 사용되는 타임스탬프와 함께 문자열을 사용할 경우, 다른 언어에서 이를 올바르게 해석하지 못할 수 있습니다.

## 관련 링크

- [Gleam 공식 사이트](https://gleam.run/)
- [Gleam 관련 패키지 목록](https://github.com/gleam-lang/awesome-gleam)
- [YAML 문서](https://yaml.org/)