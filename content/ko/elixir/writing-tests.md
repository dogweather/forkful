---
title:                "테스트 작성하기"
html_title:           "Elixir: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇을 & 왜?

프로그래머들이 테스트를 작성하는 이유는 애플리케이션의 버그를 찾고 수정하기 위해서입니다. 테스트를 작성하는 것은 코드의 품질을 향상시키고 애플리케이션의 안정성을 보장하는 중요한 단계입니다.

## 하는 방법:

```Elixir
test "나이가 18살 이상인지 체크", do: 
  assert 18 <= 18 # 이 테스트는 성공함
``` 

테스트를 작성하는 방법은 간단합니다. ```test "테스트 이름", do: ``` 블록 안에 테스트를 작성하고 해당 테스트가 정확한 결과가 나오는지 확인하기 위해 ```assert``` 문을 사용합니다.

## 깊이 파헤치기:

**역사적 배경:** 테스트 주도 개발(Test-driven development)은 민첩한 소프트웨어 개발 방법론 중 하나로, 코드를 작성하기 전에 테스트부터 작성하고 해당 테스트를 성공시켜 나가며 개발을 진행하는 방식입니다. 이는 코드의 품질을 높이고 버그를 사전에 발견하여 애플리케이션의 안정성을 확보합니다.

**대안:** 테스트 주도 개발 방법론 이외에도 다양한 테스트 방식이 존재합니다. 예를 들어, 수동 테스트(manual testing)는 사람이 직접 애플리케이션을 실행하고 기능을 테스트하는 방법입니다. 또한 인수 테스트(acceptance testing)는 애플리케이션이 사용자의 요구사항을 충족하는지를 테스트하는 방식입니다.

**구현 세부 사항:** Elixir에서 테스트를 작성하기 위해 ```ExUnit``` 모듈을 사용합니다. 이 모듈은 간단한 다중 패러다임 테스트 프레임워크로, Elixir 코드의 모든 부분을 테스트할 수 있도록 해줍니다.

## 더 많은 정보:

- [테스트에 대한 좋은 글](https://medium.com/@WEtl/how-to-write-great-elixir-tests-2b52c7c8733b)