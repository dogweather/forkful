---
aliases:
- /ko/rust/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:32.342806-07:00
description: "Rust\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\
  \uB294 \uAC83\uC740 \uBB38\uC790\uAC00 \uBB38\uC790\uC778 \uACBD\uC6B0 \uCCAB \uBC88\
  \uC9F8 \uBB38\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uACBD\uD558\uACE0 \uB098\
  \uBA38\uC9C0 \uBB38\uC790\uC5F4\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC774 \uC791\uC5C5\uC744 \uC81C\uBAA9\uC744 \uC900\uBE44\uD558\uAC70\
  \uB098 \uC0AC\uC6A9\uC790 \uC785\uB825\uC758 \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\
  \uD558\uB294 \uB4F1\uC758 \uD615\uC2DD \uC9C0\uC815 \uBAA9\uC801\uC73C\uB85C \uC218\
  \uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.858906
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\
  \uB294 \uAC83\uC740 \uBB38\uC790\uAC00 \uBB38\uC790\uC778 \uACBD\uC6B0 \uCCAB \uBC88\
  \uC9F8 \uBB38\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uACBD\uD558\uACE0 \uB098\
  \uBA38\uC9C0 \uBB38\uC790\uC5F4\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC774 \uC791\uC5C5\uC744 \uC81C\uBAA9\uC744 \uC900\uBE44\uD558\uAC70\
  \uB098 \uC0AC\uC6A9\uC790 \uC785\uB825\uC758 \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\
  \uD558\uB294 \uB4F1\uC758 \uD615\uC2DD \uC9C0\uC815 \uBAA9\uC801\uC73C\uB85C \uC218\
  \uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Rust에서 문자열을 대문자화하는 것은 문자가 문자인 경우 첫 번째 문자를 대문자로 변경하고 나머지 문자열은 변경하지 않는 것을 포함합니다. 프로그래머들은 종종 이 작업을 제목을 준비하거나 사용자 입력의 일관성을 보장하는 등의 형식 지정 목적으로 수행합니다.

## 어떻게:

Rust에서 문자열을 대문자화하려면 두 가지 주요 방법이 있습니다: 표준 라이브러리 기능을 사용하거나 더 복잡하거나 특정한 필요에 맞게 타사 크레이트를 사용합니다. 두 가지 방법을 모두 수행하는 방법은 다음과 같습니다.

### Rust의 표준 라이브러리 사용하기

Rust의 표준 라이브러리는 문자열을 대문자화하는 직접적인 방법을 제공하지 않지만, 문자열의 문자를 조작하여 이를 달성할 수 있습니다.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // 출력: Hello
}
```

### `heck` 크레이트 사용하기

더 큰 텍스트 처리 맥락 내에서 작업하는 경우, 특히 더 단순한 접근 방식을 선호한다면 `heck`와 같은 타사 라이브러리를 사용하는 것이 좋을 수 있습니다. `heck` 크레이트는 문자열을 대문자화하는 간단한 방법을 포함하여 다양한 케이스 변환 기능을 제공합니다.

먼저, `Cargo.toml`에 `heck`을 추가합니다:

```toml
[dependencies]
heck = "0.4.0"
```

그런 다음, 문자열을 대문자화하는 데 사용합니다:

```rust
extern crate heck; // Rust 2018 에디션 이후에서는 필요 없음
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // 출력: Hello World
}
```

주의: `heck`에 의해 제공되는 `to_title_case` 메소드는 문자열의 각 단어를 대문자화하는데, 이는 단지 문자열의 첫 번째 문자를 대문자화하길 원하는 경우보다 더 많을 수 있습니다. 특정한 필요에 따라 사용법을 조정하세요.
