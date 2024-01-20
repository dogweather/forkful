---
title:                "테스트 작성하기"
html_title:           "Ruby: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

테스트 작성이란 무엇인지와 프로그래머가 이를 왜 하는지에 대한 간단한 설명입니다. 

## 어떻게:

```Ruby
require "test/unit"

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    assert_equal 4, 2 + 2
  end

  def test_subtraction
    assert_equal 3, 5 - 2
  end
end
```
위 예시는 Test::Unit 라이브러리를 사용하여 간단한 계산기 테스트를 작성하는 방법을 보여줍니다. 테스트 코드를 작성할 때는 테스트 케이스가 모든 종류의 입력과 예외를 다루는지 확인하는 것이 중요합니다.

## 깊이 파헤치기:

### 역사적 배경:
테스트 작성은 소프트웨어 개발의 초기 단계부터 중요한 요소로 알려져 왔습니다. 프로그래머들은 코드를 테스트하여 예상치 못한 버그를 찾고, 유지 보수를 쉽게 하기 위해 필요한 기능들을 구현합니다.

### 대안:
Test::Unit은 오래된 라이브러리로 RSpec과 같은 새로운 테스트 도구들이 더 많이 사용됩니다. 이 도구들은 더욱 유연하고 쉽게 이해할 수 있는 테스트 코드를 작성할 수 있게 해줍니다.

### 구현 세부 사항:
테스트 작성에는 여러 가지 방법이 있지만 가장 일반적인 방법은 메소드 단위의 테스트, 클라스 단위의 테스트, 그리고 적절한 입력과 예외를 다루는 시나리오 테스트입니다. 또한 테스트하는 동안 프로그램의 상태를 수정하지 않도록 주의해야 합니다.

## 또 보기:
