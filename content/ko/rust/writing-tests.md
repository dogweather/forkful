---
title:                "프로그래밍 테스트 작성하기"
html_title:           "Rust: 프로그래밍 테스트 작성하기"
simple_title:         "프로그래밍 테스트 작성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 테스트를 작성하나요?

테스트를 작성하면 코드를 더욱 견고하고 안정적으로 만들 수 있습니다. 또한 테스트를 통해 코드의 예상치 못한 버그를 미리 발견할 수 있고, 코드를 수정할 때 기존 기능이 올바르게 작동하는지 확인할 수 있습니다. 

## 작성 방법

테스트는 `#[test]` 어노테이션을 사용하여 함수로 만들 수 있습니다. 간단한 예시는 아래와 같습니다.

``` Rust
#[test]
fn add_numbers() {
  let result = add(2, 3);
  assert_eq!(result, 5);
}
```

위 예시에서 우리는 `add()` 함수를 테스트하고, `result` 변수에는 예상되는 결과값을 저장합니다. 그리고 `assert_eq!()` 매크로를 사용하여 `result` 값이 우리가 예상한 값과 일치하는지 확인합니다. 일치하지 않는 경우 테스트는 실패합니다.

## 깊이 파고들기

테스트를 작성할 때는 다양한 테스트 케이스를 고려해야 합니다. 예를 들어, 입력 값이 음수를 포함하는 경우에도 올바르게 작동하는지 테스트해야 합니다.

또한 `#[should_panic]` 어노테이션을 사용하여 코드 실행 중 예상치 못한 에러가 발생하는 경우 테스트를 통해 이를 잘 처리하는지 확인할 수 있습니다.

## 더 보기

- [Rust 테스트 문서](https://doc.rust-lang.org/book/ch11-02-running-tests.html)
- [테스트는 왜 중요한가요?](https://agiledeveloper.com/articles/why_tests_matter.html)
- [테스트 주도 개발](https://en.wikipedia.org/wiki/Test-driven_development)