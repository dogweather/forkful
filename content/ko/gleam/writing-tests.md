---
title:                "프로그래밍 테스트 작성하기"
html_title:           "Gleam: 프로그래밍 테스트 작성하기"
simple_title:         "프로그래밍 테스트 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

제 목: Gleam 프로그래밍에서의 테스트 작성하기

## 무엇인가요? 
테스트 작성이란 무엇일까요? 단순히 말하면, 이것은 소프트웨어의 작동을 확인하는 프로그램입니다. 테스트를 작성하는 이유는 무엇일까요? 테스트를 작성함으로써 우리는 우리가 만든 코드들이 예상대로 작동하는지 확인할 수 있고, 버그를 발견할 수 있습니다. 이러한 테스트를 통해 소프트웨어의 품질을 향상시킬 수 있습니다.

## 어떻게 하나요?
Gleam에서 테스트를 작성하는 것은 매우 쉽습니다. 아래의 코드블록을 보시죠.

```Gleam
test "더하기 함수는 두 숫자를 더해줍니다" {
  assert.equal(더하기(1, 2), 3)
}

test "곱하기 함수는 두 숫자를 곱해줍니다" {
  assert.equal(곱하기(2, 3), 6)
}
```

위의 코드는 두 가지 테스트를 작성하는 예시입니다. 첫 번째 테스트는 더하기 함수가 두 숫자를 더한 값을 제대로 반환하는지를 확인하고, 두 번째 테스트는 곱하기 함수가 두 숫자를 곱한 값을 제대로 반환하는지를 확인합니다. assert.equal은 테스트 결과를 비교해주는 함수입니다.

## 깊이 알아보기
테스트 작성에는 여러가지 방법이 있지만, Gleam에서는 기본적으로 assert.equal 함수를 사용합니다. 다른 언어에서는 테스트 프레임워크를 사용하기도 합니다. 하지만 Gleam에서는 이와 같은 간단한 방식으로도 충분합니다.

## 관련 자료
- Gleam 공식 홈페이지: https://gleam.run/
- Gleam 소스 코드 저장소: https://github.com/gleam-lang/gleam