---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열에서 패턴에 일치하는 문자를 삭제하는 작업은 파싱, 데이터 정리 및 변환 등에서 중요합니다. 이를 통해 불필요한 문자를 제거하고, 오직 필요한 데이터만을 가질 수 있습니다.

## 방법:

Rust에서 문자열에서 특정 패턴에 일치하는 모든 문자를 제거하는 방법은 `replace()` 함수를 사용하는 것입니다. 예를들어 아래의 코드를 봅시다:

```Rust
fn main() {
    let s = "Hello, World!";
    let s = s.replace(",", "");
    println!("{}", s);
}
```

이 코드를 실행하면, 아래와 같은 출력을 얻게 됩니다:

```
Hello World!
```

## 깊게 알아보기

문자열에서 일치하는 패턴 삭제는 텍스트 정리 및 처리에 사용되는 전통적인 방법입니다. Rust는 이 작업을 매우 직접적이면서도 강력한 방법으로 수행할 수 있게 해줍니다.

Rust 이외에도 Python의 `replace()`, JavaScript의 `replace()` 등 다른 언어에서도 이런 기능을 지원합니다. 하지만 Rust가 더 높은 성능과 메모리 안전성, 그리고 타입 안전성을 제공한다는 점이 강점입니다.

`replace()` 함수의 실행 과정은 대략 다음과 같습니다: 함수는 문자열을 순회하면서 찾고자 하는 패턴이 있는지 검사합니다. 패턴을 찾으면 새로운 문자열에 패턴을 제외한 나머지 부분을 추가하고, 패턴을 찾지 못하면 그대로 문자열을 추가합니다.

## 참고자료

- [Rust Documentation: replace()](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rust by Example: String](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Stack Overflow: How to remove all matching characters from a string in Rust](https://stackoverflow.com/questions/52488549/how-do-i-remove-all-matching-characters-from-a-string-in-rust)