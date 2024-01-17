---
title:                "테스트 작성"
html_title:           "Rust: 테스트 작성"
simple_title:         "테스트 작성"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇인가요? & 왜 하는 걸까요?
테스트 작성이란 무엇이며, 프로그래머들이 왜 이를 수행하는지에 대한 간단한 설명입니다.

## 방법:
```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
        assert_eq!(add(0, 0), 0);
        assert_eq!(add(-1, 1), 0);
    }
}
```

테스트를 작성하는 예제와 그 결과를 ```Rust ... ``` 코드 블록 내에 제공합니다.

## 깊이 파고들기:
테스트 작성에 대한 역사적 배경, 대안 및 구현 세부 사항과 같은 더 깊은 정보입니다.

## 관련 자료
관련 자료를 위한 링크를 제공합니다.