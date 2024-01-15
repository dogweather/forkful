---
title:                "테스트 작성"
html_title:           "Fish Shell: 테스트 작성"
simple_title:         "테스트 작성"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

개발자로서, 코드의 신뢰성과 안정성은 매우 중요한 요소입니다. 그러나 만약 우리가 우리의 코드를 만드는 과정에서 테스트를 제대로 수행하지 않는다면, 우리의 코드는 버그와 결함으로 가득찰 것입니다. 이것은 우리의 애플리케이션을 비교적 예측할 수 없게 만들기 때문입니다. 이 때문에 우리는 테스트를 작성하는 것이 필요합니다.

## 법을 따르세요

우리의 코드를 테스트하기 위해 우리는 Fish Shell을 사용할 수 있습니다. Fish Shell을 이용해 간단한 예제를 살펴보겠습니다.

```Fish Shell
# 여러분의 function
function greet --argument name
  # Hello와 함께 name을 출력합니다.
  echo "Hello, $name!"
end

# 테스트 함수
function test_greet
  # "World"라는 인자와 함께 greet function을 호출합니다.
  greet World

  # 함수의 출력값을 확인합니다.
  assert_equal "Hello, World!" (commandline -c 'greet World')
end
```

위 코드에서, 우리는 `test_greet` 함수를 선언했습니다. 이 함수는 매개변수가 "World"인 `greet` 함수를 호출하고, 그 결과값을 확인합니다. 만약 우리가 `assert_equal`을 사용해 `greet` 함수가 정확한 출력값을 반환하는지 확인하지 않았다면, 이 코드는 단순히 `Hello, World!`라는 문자열을 출력하는 것이지,`Hello, World!`라는 문자열을 반환하는지 확인할 수 없습니다.

## 더 깊게

테스트를 작성하기 전에, 우리는 테스트 용도의 함수를 선언해야 합니다. 이 함수는 `test_`로 시작해야 하며, 나머지는 우리가 테스트하고 싶은 함수의 이름과 동일해야 합니다. 이렇게 함으로써 Fish Shell은 우리가 만든 테스트 함수를 실행하고, 그 결과값을 테스트하기 위해 `assert_equal`을 이용해 함수를 호출합니다. 만약 테스트 코드에서 정확한 결과값을 반환하지 않는다면, Fish Shell은 `AssertionError`를 발생시킵니다.

## 참조

- Fish Shell 공식 가이드: https://fishshell.com/docs/current/index.html
- TDD(Test-Driven Development)란?: https://www.agilealliance.org/glossary/tdd/
- 쉽게 배우는 TDD: https://www.inflearn.com/course/%EC%89%BD%EA%B2%8C-%EB%B0%B0%EC%9A%B0%EB%8A%94-tdd