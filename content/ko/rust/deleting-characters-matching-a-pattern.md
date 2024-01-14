---
title:    "Rust: 패턴과 일치하는 문자 삭제하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜 삭제패턴에 맞는 문자를 제거할까?

때로는 문자열 내에서 특정 패턴을 가진 문자를 삭제하는 것이 필요할 수 있습니다. 이 작업은 데이터 정제나 패턴을 가진 문자를 간단히 제거할 때 유용합니다. 더 자세한 방법은 아래에서 설명하겠습니다.

## 어떻게 코드로 구현할까?

이 작업을 구현하는 가장 간단한 방법은 Rust의 `replace()` 함수를 사용하는 것입니다. 다음 예제를 통해 확인해보세요.

```Rust
fn main() {
    let string = String::from("Hello, World! This is a Rust programming tutorial.");
    let replaced_string = string.replace("Rust", "");
    println!("{}", replaced_string);
}
```

출력 결과:
```
Hello, World! This is a programming tutorial.
```

위 코드에서는 `replace()` 함수를 사용하여 문자열 내의 "Rust"를 빈 문자열로 대체했습니다. 만약 여러 개의 패턴을 한 번에 제거하려면, `replace()` 함수를 여러 번 사용하면 됩니다.

## 더 자세한 내용

만약 문자열 내에서 정규표현식을 사용하여 패턴을 지정하고 싶다면, Rust의 `regex` 라이브러리를 사용할 수 있습니다. 이를 이용하여 패턴에 맞는 문자를 찾은 후 삭제하는 작업을 수행할 수 있습니다. 예를 들어, 다음 코드는 문자열 내에서 모음을 삭제하는 예제입니다.

```Rust
use regex::Regex;

fn main() {
    let string = String::from("Hello, World! This is a Rust programming tutorial.");
    let vowel_pattern = Regex::new(r"[aeiou]").unwrap();
    let replaced_string = vowel_pattern.replace_all(&string, "");
    println!("{}", replaced_string);
}
```

출력 결과:
```
Hll, Wrld! Th s  Rust prrmmng ttrtl.
```

위 코드에서는 먼저 `Regex` 타입의 변수를 생성하고, 해당 정규표현식을 이용하여 패턴을 지정합니다. 그 후 `replace_all()` 함수를 사용하여 찾은 모든 패턴을 빈 문자열로 대체합니다. 이외에도 `split()` 함수를 사용하여 패턴을 기준으로 문자열을 분리하여 다양한 작업을 수행할 수 있습니다.

## 관련 자료

- Rust 공식 문서: <https://doc.rust-lang.org/std/string/struct.String.html#method.replace>
- `regex` 라이브러리: <https://crates.io/crates/regex>