---
title:                "패턴에 일치하는 문자 삭제"
aliases: - /ko/rust/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:29.361034-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 특정 패턴에 맞는 문자 삭제하는 것은 변경되어야하는 데이터를 정리하는 과정입니다. 프로그래머들은 불필요한 데이터를 제거하거나, 입력을 깨끗하게 처리하기 위해 이 작업을 수행합니다.

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
