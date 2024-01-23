---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 코드를 작성한다는 것은 코드가 의도한 대로 작동하는지 확인하는 자동화된 방법입니다. 프로그래머는 버그를 줄이고, 코드 품질을 높이며, 나중에 코드를 변경하거나 업데이트할 때 신뢰성을 확보하기 위해 테스트를 합니다.

## How to: (어떻게 하나요?)
Gleam에서 테스트는 간단합니다. 아래는 기본적인 테스트 케이스를 보여줍니다:

```gleam
import gleam/should

pub fn add(x: Int, y: Int) -> Int {
  x + y
}

pub fn add_test() {
  should.equal(add(1, 2), 3)
}
```

테스트를 실행하면 다음과 같은 출력이 나옵니다:

```shell
$ gleam test
.
Done: passed 1; failed 0; skipped 0; total 1
```

## Deep Dive (심화학습)
테스트 작성은 소프트웨어 개발 초기부터 중요했습니다. 역사적으로, 'Test-Driven Development'(TDD) 같은 접근법이 사용되었어요. Gleam에서는 `gleam/should` 모듈을 활용해 단정(assertions)을 사용합니다. Elixir의 ExUnit이나 Erlang의 Common Test와 같은 다른 테스팅 프레임워크를 사용할 수도 있지만, `gleam/should`이 내장 모듈이고 사용하기 간단합니다.

## See Also (추가 정보)
- [Gleam GitHub 저장소](https://github.com/gleam-lang/gleam)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
