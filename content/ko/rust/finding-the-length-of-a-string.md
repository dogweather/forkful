---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것이란, 해당 문자열이 얼마나 많은 문자를 포함하는지를 확인하는 것입니다. 이를 통해 프로그래머들은 불필요한 메모리 낭비를 방지하거나, 복잡한 연산을 최적화 할 수 있습니다.

## 어떻게:

Rust에서는 `len()` 함수를 사용하여 문자열의 길이를 찾을 수 있습니다. 아래 예시를 확인해 보세요:

```Rust
fn main() {
    let my_string = String::from("안녕하세요");
    let length = my_string.chars().count();
    println!("Length: {}", length);
}
```

이 코드를 실행하면, '안녕하세요' 문자열의 길이를 출력합니다:

```Rust
Length: 5
```

## 심층 학습

문자열의 길이를 확인하는 것은 컴퓨터 사이언스의 가장 기본적인 개념 중 하나입니다. 초기 프로그래밍 언어에서부터 현대의 고급 언어까지, 이 작업은 여전히 필요한 기능입니다.

Rust에서는 두 가지 방법으로 문자열의 길이를 확인 가능합니다. 첫 번째는 위에서 살펴본 `len()` 함수를 사용하는 것이며, 두 번째 방법은 `char().count()` 함수를 사용하는 것입니다. 첫 번째 방법은 바이트 단위의 길이를 반환하므로, 유니코드 문자를 위해서는 두 번째 방법을 사용하는 것이 더 정확합니다.

## 참고 사항

더 자세한 정보, 특히 문자열과 문자열의 길이에 관한 다양한 활용법을 알고 싶다면 다음의 문서를 참고하세요:

1. Rust 공식 문서: [Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
2. Rust by Example: [Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
3. StackOverflow의 Rust 문자열에 관한 질문: [Rust - String vs str](https://stackoverflow.com/questions/24158114/what-are-the-differences-between-rusts-string-and-str)

이 외에도 Rust의 문자열 조작에 대한 다양한 주제와 문제는 구글 검색, 블로그, 깃허브 등에서 쉽게 찾아보실 수 있습니다.