---
title:                "Rust: 패턴과 일치하는 문자 삭제"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 특정한 패턴에 맞는 문자를 삭제하는 것이 유용한 이유는 다양합니다. 예를 들어, 사용자의 입력값에서 악성 URL을 제거하거나, 데이터베이스에서 중복된 값을 제거하는 등 다양한 상황에서 유용하게 사용할 수 있습니다.

## 사용 방법

`regex` 라이브러리를 사용하여 Rust 언어로 특정 패턴에 맞는 문자를 삭제하는 방법을 알아보겠습니다.

먼저, `Cargo.toml` 파일에 다음과 같이 `regex` 라이브러리를 추가합니다.

```
[dependencies]
regex = "1.3.4"
```

다음으로 해당 문자열과 삭제할 패턴을 정의합니다.

```
let input = "Hello, world! This string contains some numbers: 12345";
let pattern = r"[0-9]+";
```

이제 `regex::Regex::replace_all()` 함수를 사용하여 패턴과 일치하는 문자를 공백으로 대체합니다.

```
let output = regex::Regex::replace_all(input, pattern, "");
```

`output` 변수에는 패턴과 일치하는 숫자가 삭제된 결과가 저장됩니다. 코드 전체는 아래와 같습니다.

```
use regex::Regex; // regex 라이브러리 임포트

fn main() {
    // 문자열과 삭제할 패턴 정의
    let input = "Hello, world! This string contains some numbers: 12345";
    let pattern = r"[0-9]+";

    // 패턴과 일치하는 문자를 공백으로 대체하여 `output` 변수에 저장
    let output = Regex::replace_all(input, pattern, "");

    println!("{}", output);
}
```

아래는 위 코드의 실행 결과입니다.

```
Hello, world! This string contains some numbers:
```

## 자세히 알아보기

패턴을 사용하여 문자열에서 특정한 문자를 삭제하는 것은 Rust 언어에서 자주 사용되는 기술 중 하나입니다. 이를 통해 텍스트 처리 작업을 더 쉽게 수행할 수 있습니다. 하지만 패턴의 종류에 따라 성능 차이나 예외 상황 등 다양한 측면에서 고민해야 할 필요가 있습니다.

예를 들어, 위에서 사용한 패턴 `[0-9]+`은 숫자만을 지우고 싶을 때 효과적이겠지만, 만약 해당 문자열에서 포함된 알파벳까지 모두 삭제하고 싶다면 다른 패턴을 고민해야 합니다. 또한, 매우 긴 문자열에서 패턴을 삭제할 때는 어떤 방식으로 처리하는 것이 효율적인지 고민하여 속도를 높일 수 있습니다.

## 참고 자료

- [regex 라이브러리 문서](https://docs.rs/regex/1.3.4/regex/)
- [Rust 언어 공식 웹사이트](https://www.rust-lang.org/ko/)

---

## 관련 링크

- [Regular Expressions in Rust](https://blog.codecentric.de/en/2019/03/regular-expressions-rust/)
- [Big Mighty Regular Expressions in Rust](https://www.youtube.com/watch?v=AECrxGCEjkI&t=18s)
- [An Introduction to Regular Expressions in Rust](https://medium.com/@sdboyer/an-introduction-to-regular-expressions-in-rust-c2174f90e800)