---
title:                "테스트 작성하기"
html_title:           "Gleam: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성해야 할까요?프로그램을 작성할 때 컴퓨터에서 시험 하지 않고 실행되지 않는다면, 버그가 발생할 수 있습니다. 테스트를 작성하면 이러한 문제를 미리 발견하여 프로그램의 안정성을 보장할 수 있습니다. 테스트를 작성함으로써 더 나은 소프트웨어를 만들기 위해 반복적인 개발과 수정을 할 수 있습니다.

## 어떻게 테스트를 작성할 수 있나요?

Gleam은 ```test``` 블록을 사용하여 간단하고 효율적인 테스트를 작성할 수 있게 해줍니다. 다음은 함수 ```sum```을 테스트하는 예제입니다.

```Gleam
test "sum 함수는 두 숫자의 합을 반환해야 합니다." {
  let result = sum(5, 7)
  assert result == 12
}
```

위 코드에서 테스트 블록은 개발자가 기대하는 기능을 설명하고, 테스트할 코드를 작성하고, ```assert```를 사용하여 기대하는 결과를 확인합니다. 이를 실행하면 ```sum``` 함수가 올바르게 작동하는지 확인할 수 있습니다.

## 깊게 파고들어보기

테스트를 작성할 때 중요한 점은 모든 경우의 수를 고려하는 것입니다. 예를 들어, ```sum``` 함수에 음수가 입력될 경우를 테스트하는 것도 중요합니다. Gleam을 사용하면 테스트 블록을 작성하고 실행하는 것이 간단하고 쉽습니다. 이를 통해 모든 경우의 수를 고려하여 안정적인 소프트웨어를 개발할 수 있습니다.

## 자세히 알아보기

- [Gleam 공식 문서](https://gleam.run/documentation/)

[See Also]
- [Gleam 테스트 패턴](https://medium.com/@lukewestby/gleam-testing-patterns-cabdd5f382d0)
- [테스트 주도 개발과 Gleam](https://krsehh.tistory.com/45)
- [테스트 기반 개발의 장단점](https://ncube.net/ncube/blog/view.do?postUid=1106)