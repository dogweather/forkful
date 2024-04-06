---
date: 2024-01-20 17:43:29.361034-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) Rust\uC5D0\uC11C\uB294 `replace` \uB610\
  \uB294 \uC815\uADDC \uD45C\uD604\uC2DD `Regex::replace`\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.689490-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) Rust\uC5D0\uC11C\uB294 `replace` \uB610\uB294 \uC815\
  \uADDC \uD45C\uD604\uC2DD `Regex::replace`\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD328\
  \uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (어떻게:)
Rust에서는 `replace` 또는 정규 표현식 `Regex::replace`를 사용하여 패턴에 일치하는 문자를 삭제할 수 있습니다.

```Rust
use regex::Regex;

fn main() {
    let text = "FooBarBaz";
    
    // "Bar"를 삭제
    let cleaned = text.replace("Bar", "");
    println!("{}", cleaned); // 출력: FooBaz
    
    // 정규 표현식 사용해 "a" 다음에 "r"이 오는 패턴을 삭제
    let re = Regex::new("a.r").unwrap();
    let result = re.replace_all(text, "");
    println!("{}", result); // 출력: FooBaz
}
```

## Deep Dive (깊은 이해)
Rust에서 문자열 조작은 많은 다양한 프로그래밍 언어들과 비슷합니다. 2010년 Rust의 첫 등장 이후로, `String`과 `str` 타입들은 효율적으로 텍스트 데이터를 다루도록 계속 발전해왔습니다. `replace` 함수는 간단한 매칭에 사용되고 `Regex::replace`는 복잡한 패턴을 다룰 때 사용됩니다. `regex` 크레이트는 Rust에서 정규 표현식을 사용하기 위한 강력한 도구를 제공합니다. 효율성과 유연성이 필요할 때 정규 표현식은 좋은 선택입니다.

## See Also (더 보기)
- Rust `String` documentation: https://doc.rust-lang.org/std/string/struct.String.html
- `regex` crate documentation: https://docs.rs/regex/
- 정규 표현식에 대한 학습: https://www.regular-expressions.info/
