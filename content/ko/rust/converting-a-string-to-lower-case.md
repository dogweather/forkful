---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열(string)을 소문자(lower case)로 변환하는 것은 문자열에 있는 모든 대문자로 구성된 알파벳 문자를 그에 해당하는 소문자로 바꾸는 작업입니다. 이를 통해 프로그래머들은 데이터 정규화 및 문자열 비교 작업을 쉽게 수행할 수 있습니다.

## 어떻게 하는가:

Rust에서 문자열을 소문자로 변환하는 법은 아래 예제에서 확인할 수 있습니다:
```Rust
fn main() {
    let s = "Hello World!";
    println!("{}", s.to_lowercase());
}
```

위 코드의 출력은 다음과 같습니다:
``` 
hello world!
```

## 깊게 파보기:

Rust는 국제화에 중점을 둔 프로그램을 작성하는데 적합한 도구로서, 소문자 변환 기능도 이를 지원합니다. 이것은 문자열의 각 문자에 대해 소문자 문자를 찾는 Unicode의 규칙을 따릅니다. 

다른 언어에서는 단순화된 작업을 수행하거나, 소문자 변환을 수행하는 데 있어 특정 언어 문맥에 민감한 복잡한 알고리즘을 사용할 수 있습니다. Rust의 `to_lowercase()` 함수는 일관성을 유지하면서 다양한 언어 범위의 소문자 변환을 지원하기 위해 국제적인 컨텍스트를 고려합니다.

변환 작업에서 발존할 수 있는 세부 문제 중 하나는, 일부 유니코드 문자가 소문자 변환 시 길이가 달라질 수 있다는 것입니다. `to_lowercase()` 함수는 새 String을 반환하므로, 이 문제를 깔끔하게 처리합니다.

## 참고하십시오:

다음은 주제에 대한 추가 정보를 찾을 수 있는 몇 가지 관련 소스입니다:

1. Rust std::string::String 문서: [to_lowercase() 구현](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)

2. 모범 사례: [Unicode 준수](https://unicode.org/reports/tr21/)

3. Rust 커뮤니티에게 물어보기: [Rust 사용자 포럼](https://users.rust-lang.org/)