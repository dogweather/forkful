---
title:                "Rust: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
컴퓨터 프로그래밍에서 많은 경우, 특정한 패턴에 맞는 문자들을 삭제해야 할 필요가 있습니다. 이 글에서는 러스트 프로그래밍 언어를 사용하여 문자 패턴에 맞는 문자들을 삭제하는 방법을 알아보겠습니다.

## 어떻게
우선, 문자 패턴을 정의하기 위해 `regex` 라이브러리를 사용해야 합니다. 이 라이브러리를 사용하면 우리는 정규 표현식으로 원하는 패턴을 지정할 수 있습니다. 아래의 예시 코드를 살펴보세요.

```rust
use regex::Regex;

fn main() {
    let pattern = Regex::new(r"[0-9]+").unwrap(); // 정수를 찾는 패턴
    let text = "abc123def456"; // 패턴을 찾을 문자열
    let replaced_text = pattern.replace_all(text, ""); // 패턴에 맞는 문자들을 삭제
    println!("삭제된 결과: {}", replaced_text); // 결과: abcdef
}
```

위의 예시 코드에서는 `regex` 라이브러리를 사용하여 문자열에서 숫자를 찾는 패턴을 지정하고, `replace_all` 함수를 사용하여 해당 패턴에 맞는 문자들을 모두 삭제하였습니다.

## 깊게 파고들기
자세히 살펴보면, `replace_all` 함수는 더 많은 기능을 제공합니다. 예를 들어, `replace_all` 함수의 두 번째 인자로는 삭제 대신 다른 문자열을 넣어줄 수도 있습니다. 또한, `replace_all` 함수 대신 `replace` 함수를 사용하면 한 번만 매칭되는 패턴에 대해서 삭제나 다른 문자열로 대체할 수 있습니다. 더 자세한 정보는 러스트 공식 문서를 참고해주세요.

## 더 알아보기
- [regex 라이브러리 공식 사이트](https://docs.rs/regex/1.4.2/regex/)
- [러스트 공식 문서 - 정규 표현식](https://doc.rust-lang.org/std/str/struct.Regex.html)
- [러스트 공식 문서 - 문자열 처리](https://doc.rust-lang.org/book/ch08-03-hash-maps.html#storing-keys-and-values-of-different-types-in-one-hash-map)