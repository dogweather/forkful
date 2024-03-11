---
date: 2024-01-20 17:39:24.739796-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uC758 \uBAA8\uB4E0 \uB300\uBB38\uC790\uB97C\
  \ \uD574\uB2F9\uD558\uB294 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574\
  \ \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uB294 \uBE44\uAD50\
  \uB97C \uD558\uAC70\uB098, \uB370\uC774\uD130\uB97C \uC77C\uAD00\uB41C \uD615\uC2DD\
  \uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:28.816846-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740 \uD14D\uC2A4\uD2B8\uC758 \uBAA8\uB4E0 \uB300\uBB38\uC790\uB97C \uD574\
  \uB2F9\uD558\uB294 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uB300\
  \uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uB294 \uBE44\uAD50\uB97C\
  \ \uD558\uAC70\uB098, \uB370\uC774\uD130\uB97C \uC77C\uAD00\uB41C \uD615\uC2DD\uC73C\
  \uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환하는 것은 텍스트의 모든 대문자를 해당하는 소문자로 바꾸는 것입니다. 프로그래머들은 이를 통해 대소문자를 구분하지 않는 비교를 하거나, 데이터를 일관된 형식으로 저장하기 위해 사용합니다.

## How to: (방법)
```Rust
fn main() {
    let text = "Rust Programming!";
    let lowercased = text.to_lowercase();
    println!("Original: {}", text);
    println!("Lowercased: {}", lowercased);
}
```

출력 결과:
```
Original: Rust Programming!
Lowercased: rust programming!
```

## Deep Dive (심층 분석)
Rust에서 `.to_lowercase()` 메서드는 유니코드를 지원하기 때문에, 모든 언어의 문자를 제대로 소문자로 바꿀 수 있습니다. 예전 ASCII 기반 함수들과 비교하면 이는 큰 진보입니다. Rust의 표준 라이브러리는 이 메서드를 `str` 타입에 구현해두었으며, 이는 메모리 할당을 포함할 수 있습니다. 메모리 할당을 피하고 싶다면 소문자 변환을 위한 더 저수준의 접근 방법도 있지만, 그러면 유니코드 호환성에 주의를 기울여야 합니다.

무거운 작업을 피하려면 `to_ascii_lowercase`를 사용할 수 있지만, 영문자에 한해서만 작동하고 다른 유니코드 문자는 그대로 둡니다. 즉, 영어 애플리케이션에서는 괜찮지만, 국제화된 소프트웨어에서는 적합하지 않습니다.

## See Also (참고자료)
- [Rust Documentation for to_lowercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Rust by Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Unicode Case Folding](https://www.unicode.org/reports/tr21/)

Rust 자체 문서나 Rust by Example 같은 리소스는 Rust 문자열 핸들링에 대해 더 심화된 내용을 제공합니다. 유니코드 케이스 폴딩에 대해서는 유니코드 컨소시엄의 기술 보고서를 참고하세요.
