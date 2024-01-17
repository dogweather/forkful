---
title:                "정규식 사용하기"
html_title:           "Rust: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Rust에서 정규 표현식 사용하기

## 무엇 & 왜?
정규 표현식 사용이란 무엇인지 알고 싶으세요? 그래도 알아두면 좋은 이유가 있어요. 정규 표현식은 문자열에서 원하는 패턴을 찾아내는 데 사용되며, 이를 통해 프로그래머는 작업을 더 쉽고 정확하게 수행할 수 있습니다.

## 사용 방법:
아래에 있는 예제 코드를 통해 정규 표현식의 사용법을 살펴볼게요. 코드 블록을 참고하시면서 결과를 확인해보세요.

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"ab+c").unwrap();
    let text = "abc, ac, abbbc";
    for caps in re.captures_iter(text) {
        println!("Found match: {}", caps.get(0).unwrap().as_str());
    }
}
```

결과는 다음과 같습니다:
```
Found match: abc
Found match: abbbc
```

위 코드는 정규 표현식 "ab+c"에 대해 문자열 "abc, ac, abbbc"를 검색하고 일치하는 부분을 찾아내는 예제입니다.

## 깊게 들어가기:
- 정규 표현식은 1960년대에 켄 톰슨이 발명했으며, 그 이후로 많은 언어들에서 사용되고 있습니다.
- Rust에서 정규 표현식을 사용하는 대안으로는 glob 패턴 매칭이 있습니다.
- Rust의 정규 표현식은 Rust 표준 라이브러리에서 제공되는 regex 라이브러리를 사용합니다.

## 관련 자료:
- [Rust regex 라이브러리](https://docs.rs/regex/1.4.2/regex/)
- [Rust glob 패턴 매칭](https://doc.rust-lang.org/glob/)