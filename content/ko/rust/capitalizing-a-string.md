---
title:    "Rust: 문자열 대문자로 변환하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것이 중요한 이유는 다른 프로그래밍 언어보다 더 안전하고 효율적인 방식으로 문자열을 다루기 때문입니다. 러스트의 강력한 타입 시스템을 활용하여 문자열을 대문자로 변환함으로써 버그와 에러를 방지할 수 있습니다.

## 어떻게

```Rust
fn capitalize(s: &str) -> String {
    s.to_uppercase()
}

fn main() {
    let name = "john";
    let result = capitalize(name);
    println!("{}", result);
}
```

위의 코드 예제와 결과를 보면, `john`이라는 문자열이 `JOHN`으로 변환되었음을 알 수 있습니다. `to_uppercase()` 함수는 문자열을 대문자로 변환하는 메서드이며, 더 복잡한 변환 방식을 원한다면 러스트의 다른 메서드를 사용할 수도 있습니다.

## 깊은 이해

문자열을 대문자로 변환하기 위해서는 모든 문자를 단순히 대문자로 바꾸는 것이 아니라 해당 언어의 대/소문자 규칙을 고려해야 합니다. 예를 들어, 영어에서는 알파벳 'a'를 대문자로 바꾸면 'A'가 되지만, 독일어에서는 'ß'를 대문자로 바꾸면 'SS'가 됩니다. 러스트에서 제공하는 `to_uppercase()` 함수는 이러한 다양한 언어의 규칙을 처리할 수 있도록 설계되어 있습니다.

## 관련 자료

- [Rust 문자열 처리 관련 문서](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust 방대한 표준 라이브러리 문서](https://doc.rust-lang.org/std/index.html)
- [러스트 강의: 문자열 처리](https://www.youtube.com/watch?v=PzEox3szeRc)