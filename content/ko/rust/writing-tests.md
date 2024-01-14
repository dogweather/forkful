---
title:                "Rust: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성해야 할까요?

테스트를 작성하는 것은 러스트 프로그램의 안전성을 보장하기 위해 매우 중요합니다. 예상치 못한 오류를 방지하고 코드를 검증하는 데 도움이 됩니다. 또한 테스트 작성은 코드를 수정할 때 이전과 동일한 기능을 유지하기 위해 필수적입니다.

## 테스트 작성하는 방법

우리는 `assert_eq!` 함수를 사용하여 값이 예상한 것과 동일한지 확인할 수 있습니다. 아래 예시를 통해 자세하게 알아보겠습니다.

```Rust
fn add(x: i32, y: i32) -> i32 {
    return x + y;
}

#[test]
fn test_add() {
    let result = add(2, 4);
    assert_eq!(result, 6);
}
```

위 코드에서 우리는 `test_add` 함수를 정의하여 `add` 함수의 결과가 우리가 기대하는 값인 6과 동일한지 확인합니다. 만약 값이 다르다면 테스트는 실패하게 됩니다.

## 테스트 작성에 대해 깊게 알아보기

테스트를 작성하는 것은 매우 중요하지만, 실제로 작성하기는 쉽지 않을 수 있습니다. 이를 위해서는 코드 베이스와 프로젝트의 기능을 잘 이해하고, 어떤 경우에 어떤 종류의 테스트를 작성해야 하는지 알고 있어야 합니다. 효율적이고 신뢰할 수 있는 테스트를 작성하기 위해서는 모범 사례를 따르는 것이 좋습니다. 이를 통해 쉽게 오류를 찾고 수정할 수 있습니다.

## 참고 링크

- [러스트 공식 문서 - 테스트하기](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [러스트 테스트 - 배우기](https://learning-rust.github.io/docs/a7.test.html)
- [러스트 코드 베이스에서 테스트 작성하기](https://olivierlacan.com/posts/a-closer-look-at-rust-code-base-testing/)
- [러스트 테스트 모범 사례](https://doc.rust-lang.org/stable/book/appendix-07-automated-tests.html#unit-tests)