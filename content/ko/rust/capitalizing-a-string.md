---
title:                "Rust: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

코딩을 즐기는 많은 사람들 중에서도 러스트 프로그래밍 언어를 즐겨 사용하는 이들이 많습니다. 러스트는 안정성과 속도를 갖춘 새로운 언어로서, 개인 프로젝트부터 대규모 프로젝트까지 다양한 분야에서 사용되고 있습니다. 이번 글에서는 러스트를 사용해 문자열을 대문자로 바꾸는 방법에 대해 알아보겠습니다.

## 왜 대문자로 바꾸어야 할까요?

문자열을 대문자로 바꾸는 이유는 다양합니다. 예를 들어, 사용자로부터 입력을 받을 때 대문자로 입력된 경우에 대한 처리를 해야 할 수도 있고, 특정 API나 시스템에서 대문자로 된 문자열만을 인식하는 경우가 있을 수 있습니다. 또는 데이터베이스에서 대문자로 된 문자열을 비교하고자 하는 경우 등 다양한 상황에서 문자열을 대문자로 바꾸는 것이 필요할 수 있습니다.

## 어떻게 대문자로 바꿀 수 있을까요?

러스트에서는 표준 라이브러리인 `String` 타입에 `to_uppercase()` 메서드를 제공하고 있습니다. 이 메서드는 호출한 문자열을 대문자로 바꾼 새로운 `String` 타입의 값을 반환합니다. 예를 들어, 아래와 같이 사용할 수 있습니다.

```Rust
let str = "hello world";
let uppercase_str = str.to_uppercase();
println!("Uppercase string: {}", uppercase_str);

// Output:
// Uppercase string: HELLO WORLD
```

또는 `STRING` 타입에 있는 `to_lowercase()` 메서드를 사용하면 문자열을 소문자로 바꾸는 것도 가능합니다.

## 깊게 파헤쳐보기

하지만 이렇게만 사용한다면 매번 새로운 문자열을 생성하는 것이기 때문에 메모리를 낭비할 수 있습니다. 러스트에서는 문자열을 수정하는 메서드인 `make_ascii_uppercase()`와 `make_ascii_lowercase()`도 제공하고 있습니다. 이 메서드는 기존 문자열을 수정하여 대문자로 변경하거나 소문자로 변경합니다. 따라서 새로운 메모리를 할당하지 않아도 되기 때문에 성능상 이점을 가져갈 수 있습니다. 하지만 이 메서드들은 ASCII 문자만을 대상으로 사용할 수 있기 때문에 주의해야 합니다.

## 관련 자료들

- 러스트 공식 문서: https://doc.rust-lang.org/std/string/struct.String.html
- AsciiExt 트레이트: https://doc.rust-lang.org/std/ascii/trait.AsciiExt.html
- 러스트 튜토리얼: https://rinthel.github.io/rust-lang-book-ko/string.html

**참고 자료: https://doc.rust-lang.org/std/string/struct.String.html**