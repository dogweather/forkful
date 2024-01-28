---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:10:47.548921-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇을, 왜?
코드를 함수로 구성한다는 것은 프로그램의 동작을 더 작고, 재사용 가능한 조각으로 나누는 것을 의미합니다. 프로그래머들은 코드를 더 명확하게 하고, 유지보수를 쉽게 하며, 반복을 피하기 위해 이렇게 합니다.

## 방법:
Gleam에서 코드를 함수로 구성하는 간단한 예제입니다:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let sum = add(3, 4)
  sum
}

// 샘플 출력
// 7
```

이 코드 조각에서 `add`는 두 값을 받아 더하는 함수입니다. `main`은 `add`를 호출하고 그 결과를 관리하는 곳입니다.

## 심화 학습
역사적으로 함수(또는 '서브루틴')의 개념은 1960년대 이후 구조화된 프로그래밍의 길을 열며 프로그래밍을 혁신적으로 변화시켰습니다. 함수는 문제를 하위 문제로 나누고, 독립적으로 해결한 다음, 이를 조합하여 더 큰 문제를 해결하는 모듈 방식을 장려합니다.

Gleam은 강력한 타입을 가지고 있으며, 함수도 타입 정보를 가집니다. 이는 그 사용이 정의와 일관되도록 보장하여 오류를 줄이고 의도를 명확히 합니다. 

함수의 대안으로는 로직을 반복적으로 작성하는 인라인 코딩이 있습니다. 작은 일회성 작업의 경우에는 때때로 더 빠르지만, 인라인 코딩은 대규모 애플리케이션에 적합하지 않습니다. 

함수로 구성할 때 고려할 구현 세부 사항에는 함수를 구성 요소로 사용하는 함수 구성, 다른 함수를 인수로 취하거나 반환하여 코드의 구성과 실행에 유연성을 더하는 고차 함수 등이 포함될 수 있습니다.

## 참고
Gleam에서의 함수에 대해 더 알아보려면 공식 문서를 참고하세요:
- [Gleam 언어 함수](https://gleam.run/book/tour/functions.html)

또는 더 넓은 프로그래밍 개념을 탐색해 보세요:
- [Mozilla 개발자 네트워크의 자바스크립트 함수](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Functions)
- [잘 배워 봅시다 Erlang! - 모듈과 함수에 관하여](https://learnyousomeerlang.com/modules)
