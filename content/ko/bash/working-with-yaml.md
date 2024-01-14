---
title:                "Bash: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜 YAML을 사용해야 할까요?

YAML은 많은 장점을 가진 인기있는 데이터 표현 언어입니다. 이것은 간결하고 사용하기 쉬우며, 마크업이나 기타 복잡한 표현 언어보다 더 유지 관리하기 쉽습니다. YAML은 프로젝트에서 필요한 모든 설정, 데이터 및 구성을 저장하고 관리하는 데 매우 유용합니다.

## 어떻게 사용하나요?

YAML을 사용하여 소프트웨어 프로젝트에서 설정 파일을 만드는 것은 매우 간단합니다. 먼저 YAML을 작성할 수 있는 적절한 텍스트 에디터가 있어야 합니다. 그런 다음 `yml` 또는 `yaml` 확장자로 파일을 저장해야 합니다. 이제 우리가 YAML 언어의 가장 기본적인 요소를 배워보겠습니다.

```Bash
# 문자열
name: John Doe

# 숫자
age: 30

# 리스트
favorite_fruits:
  - apple
  - banana
  - orange

# 네스트된 맵
address:
  street: 123 Main St
  city: Seoul
  country: South Korea
```

YAML은 들여쓰기를 사용하여 데이터의 구조를 정의하기 때문에 매우 가독성이 높습니다. 여러분이 YAML을 잘 사용할 수 있다면 여러분의 프로젝트는 더 쉽고 효율적으로 관리될 수 있습니다.

## 깊이 있는 살펴보기

YAML에는 여러 가지 고급 기능이 있습니다. 예를 들어, 여러 파일에 걸쳐 설정을 저장하고 불러올 수도 있습니다. 또한 여러 파일 간의 참조를 사용하여 데이터를 더 잘 구조화할 수 있습니다. 마지막으로 YAML 각 항목은 유연한 데이터 타입을 가지고 있기 때문에, 데이터를 더 쉽게 관리하고 사용할 수 있습니다.

## 또 다른 참고자료

YAML에 대해 더 깊이 알아보고 싶다면 아래의 링크들을 참고해보세요.

- [YAML 공식 문서](http://yaml.org/)
- [YAML 튜토리얼](https://www.wooptoot.com/yaml1.html)
- [YAML 사용 예제](https://blog.red-badger.com/2019/06/27/yml-yaml-a-simple-developers-guide)