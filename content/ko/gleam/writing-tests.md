---
title:    "Gleam: 프로그래밍 테스트 작성하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성할까요?

일반적으로 모든 프로그래밍 언어와 프레임워크에서 코드를 작성할 때 테스트를 작성하는 것이 좋습니다. 테스트를 작성하면 코드를 변경할 때마다 그 작동 여부를 확인할 수 있고 실제 코드를 실행하기 전에 오류를 찾는 것이 가능합니다. 이는 시간과 노력을 절약하는 데에 도움이 되며 코드의 품질을 향상시킵니다.

## 테스트 작성하는 법
```Gleam
  test "더하기 함수 테스트" {
    let result = 더하기(2,3)
    assert.equal(result, 5)
  }

  fn 더하기(x,y) {
    x + y
  }
```
위의 코드 블록은 Gleam 언어로 작성된 간단한 더하기 함수 테스트 예시입니다. 사용하신 언어나 프레임워크에 따라 문법은 달라질 수 있지만 일반적인 형식은 유사합니다. 함수의 예상 결과가 실제 결과와 일치하는지를 확인하는 것이 중요합니다.

## 깊게 들어가보기

테스트를 작성하는 것은 단순히 코드의 작동 여부를 확인하는 것 이상의 의미를 가지고 있습니다. 내부 구조를 테스트해보는 과정에서 코드의 결함을 발견하고 개선하는 데에도 도움이 됩니다. 또한, 테스트 코드를 작성하면 알고리즘을 이해하는 과정에서도 도움이 되며, 이는 다른 개발자들과의 협업에 중요한 요소가 됩니다. 깊이 있는 테스트를 작성함으로써 더 나은 코드를 작성할 수 있습니다.

## 더 알아보기
Markdown 문법: https://www.markdownguide.org/basic-syntax/
Gleam 언어 공식 문서: https://gleam.run/
테스트 코드 작성 팁: https://www.softwaretestinghelp.com/how-to-write-good-test-cases-sample-template-example/