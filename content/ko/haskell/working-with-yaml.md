---
title:                "yaml을 이용한 작업"
html_title:           "Haskell: yaml을 이용한 작업"
simple_title:         "yaml을 이용한 작업"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇인가요? & 왜하는 거죠?
YAML 작업이란 무엇이며, 프로그래머들이 왜 이를 수행하는지에 대해 알아보겠습니다.

YAML은 "염치없는 마크업 언어"로, 간결하고 가독성이 높은 데이터 직렬화 형식입니다. 많은 프로그래머들이 YAML을 사용하는 이유는 텍스트 파일을 사용해 작성하기 쉽고, 데이터 객체를 표현하기에 적합하기 때문입니다.

## 어떻게 하나요?
아래처럼 ```Haskell ... ``` 코드 블록 안에 코딩 예제와 샘플 출력을 제공합니다.

```Haskell
data Person = Person { name :: String, age :: Int, occupation :: String }
```

```Haskell
- name: Bob
  age: 30
  occupation: Engineer
```

## 깊게 들어가보기
YAML 작업의 역사적 배경, 대안들, 그리고 구현 세부 정보를 살펴보겠습니다.

YAML은 2001년에 처음 개발되었으며, 프로그래머들이 XML보다 더 효율적으로 데이터를 직렬화하고 읽고 쓸 수 있도록 해주는 것을 목적으로 만들어졌습니다. YAML의 대안으로는 JSON이 있지만, YAML은 텍스트 포맷이므로 사람이 읽고 쓰기에 더 적합합니다.

## 더 알아보기
관련 자료를 참고할 수 있는 링크를 제공합니다.

[Official YAML Documentation](https://yaml.org/spec/1.2/spec.html)

[Learn X in Y minutes - YAML](https://learnxinyminutes.com/docs/yaml/)