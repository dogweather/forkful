---
date: 2024-01-20 17:39:24.739796-07:00
description: "How to: (\uBC29\uBC95) Rust\uC5D0\uC11C `.to_lowercase()` \uBA54\uC11C\
  \uB4DC\uB294 \uC720\uB2C8\uCF54\uB4DC\uB97C \uC9C0\uC6D0\uD558\uAE30 \uB54C\uBB38\
  \uC5D0, \uBAA8\uB4E0 \uC5B8\uC5B4\uC758 \uBB38\uC790\uB97C \uC81C\uB300\uB85C \uC18C\
  \uBB38\uC790\uB85C \uBC14\uAFC0 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\uC804 ASCII\
  \ \uAE30\uBC18 \uD568\uC218\uB4E4\uACFC \uBE44\uAD50\uD558\uBA74 \uC774\uB294 \uD070\
  \ \uC9C4\uBCF4\uC785\uB2C8\uB2E4. Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB294 \uC774 \uBA54\uC11C\uB4DC\uB97C `str`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.692684-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Rust\uC5D0\uC11C `.to_lowercase()` \uBA54\uC11C\uB4DC\uB294\
  \ \uC720\uB2C8\uCF54\uB4DC\uB97C \uC9C0\uC6D0\uD558\uAE30 \uB54C\uBB38\uC5D0, \uBAA8\
  \uB4E0 \uC5B8\uC5B4\uC758 \uBB38\uC790\uB97C \uC81C\uB300\uB85C \uC18C\uBB38\uC790\
  \uB85C \uBC14\uAFC0 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

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
