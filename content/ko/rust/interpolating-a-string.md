---
title:                "문자열 보간하기"
html_title:           "Rust: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

#이게 뭐고 왜 하는 걸까?
## 무엇인가요?
문자열 내부에 변수 값을 삽입하는 것을 문자열 삽입(interpolating a string)이라고 합니다. 프로그래머들은 이를 사용하여 변수 값을 동적으로 문자열에 추가할 수 있습니다.

## 어떻게 하나요?
```Rust
let name = "John";
let age = 25;
println!("안녕하세요, 내 이름은 {}이고 {}살입니다!", name, age);
```
출력:
```
안녕하세요, 내 이름은 John이고 25살입니다!
```

## 깊게 들어가보기
### 역사적 배경
문자열 삽입은 C 언어에서 시작되었으며, 여러 다른 프로그래밍 언어에서 채택되었습니다. Rust에서는 중괄호({}) 안에 변수 이름을 넣어서 문자열 삽입을 할 수 있습니다.

### 대안
문자열 삽입을 수행하기 위해 Rust에서는 또 다른 방법인 "문자열 포매팅(string formatting)"을 사용할 수 있습니다. 이 방법은 `format!` 매크로를 사용하여 동적 문자열을 만들 수 있도록 해줍니다.

### 구현 세부사항
Rust에서 문자열 삽입은 `println!` 매크로와 비슷한 방식으로 작동합니다. 중괄호 안에 변수 이름을 넣어서 해당 변수의 값을 문자열로 대체합니다.

## 관련 자료
- [Rust 공식 문서 - 문자열 삽입](https://doc.rust-lang.org/std/fmt/#fill-and-alignment)
- [Rust 공식 문서 - 문자열 포매팅](https://doc.rust-lang.org/std/fmt/#fill-and-alignment)