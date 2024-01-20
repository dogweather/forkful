---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 그리고 왜죠?)
문자열 대문자화는 문자열의 문자들을 대문자로 변환하는 것입니다. 주로 텍스트를 일관되게 표시하거나 중요한 단어를 강조하기 위해 사용됩니다.

## How to: (어떻게 하죠?)
```Rust
fn main() {
    let greeting = "hello";
    println!("Capitalize: {}", greeting.to_uppercase());
}
```
출력:
```
Capitalize: HELLO
```

## Deep Dive (깊은 곳으로)
Rust에서 문자열을 대문자로 변환하는 것은 표준 라이브러리의 `to_uppercase()` 함수를 이용하는 것을 말합니다. 역사적으로, 대문자화는 문서에서 제목이나 중요한 단어를 표시할 때 시작되었습니다. 다른 프로그래밍 언어에서도 비슷한 기능이 있지만, Rust의 `to_uppercase()`는 유니코드를 완벽히 지원하여 여러 언어로 작성된 텍스트도 정확히 대문자로 바꿉니다. 대안으로는, 더 세밀한 제어를 원할 경우에는 `chars()` 메서드로 문자열을 순회하면서 각 문자를 대문자로 변환하는 방법도 있습니다.

## See Also (더 보기)
- Rust 문자열 API 문서: [https://doc.rust-lang.org/std/string/struct.String.html](https://doc.rust-lang.org/std/string/struct.String.html)
- Unicode에 대한 자세한 정보: [https://www.unicode.org](https://www.unicode.org)
- Rust 프로그래밍에 대한 추가 학습 자료: [https://www.rust-lang.org/learn](https://www.rust-lang.org/learn)