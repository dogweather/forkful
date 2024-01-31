---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
simple_title:         "정규 표현식 활용하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 그리고 왜?
정규 표현식은 문자열에서 패턴을 찾기 위한 도구다. 프로그래머는 텍스트를 검증, 파싱, 추출, 변환할 때 정규 표현식을 사용한다.

## 사용 방법:
```Rust
use regex::Regex;

fn main() {
    let text = "안녕하세요, Rust에서 문자열 검색을 해봅시다!";
    let pattern = r"문자열";

    let re = Regex::new(pattern).unwrap();

    println!("'{}'가 포함되어 있는지? {}", pattern, re.is_match(text));
    //출력: '문자열'가 포함되어 있는지? true
}
```

## 심화 학습:
정규 표현식은 1950년대 초에 수학자 스티븐 클리니에 의해 도입되었다. 대안으로 문자열 검색 메소드나 파싱 라이브러리를 사용할 수 있다. Rust에서는 regex 크레이트를 이용하며, 이는 Rust의 속도와 안전성을 활용해 효율적인 패턴 매칭을 제공한다.

## 참조:
- Rust regex 문서: https://docs.rs/regex/latest/regex/
- 정규 표현식에 대한 추가 학습: https://www.regular-expressions.info/
- Rust 문자열 처리: https://doc.rust-lang.org/book/ch08-02-strings.html
