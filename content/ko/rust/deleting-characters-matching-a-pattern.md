---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Rust: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 이유
Rust는 안전성, 성능, 간결성을 강조하는 언어로, 많은 프로그래머들이 이를 선택합니다. 이 중에서도 Rust에서 제공하는 문자열 처리 기능은 매우 강력하고 효율적입니다. 따라서 Rust를 사용하는 프로그래머라면 문자열 처리를 다루는 방법을 알아두는 것이 중요합니다.

## 방법
Rust에서 문자열을 다루는 가장 기본적인 방법은 `str` 타입의 메소드를 이용하는 것입니다. 여기서는 특정 패턴과 일치하는 문자들을 삭제하는 방법을 알아보겠습니다. 먼저, 다음과 같은 문자열이 있다고 가정해보겠습니다.
```
let string = "Hello, World!";
```
이제 `string`에서 모음을 삭제하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.
```
let new_string = string.chars().filter(|c| !c.is_vowel()).collect::<String>();
```
우선 `.chars()` 메소드를 사용하여 `string`을 각각의 문자로 나누고, `.filter()` 메소드를 이용하여 모음인지 아닌지를 확인합니다. 모음이 아니면 `true`를 반환하여 필터링하고, `.collect()` 메소드를 이용하여 `String` 타입으로 다시 합쳐주는 것입니다. 이렇게 하면 `new_string`에는 모음이 삭제된 "Hll, Wrld!" 문자열이 저장됩니다.

## 깊이 살펴보기
Rust에서 제공하는 문자열 처리 기능중 가장 유용한 것은 `Regex` 모듈입니다. 이를 이용하면 정규식을 사용하여 문자열을 다룰 수 있습니다. 다음은 `Regex`를 이용하여 패턴과 일치하는 문자를 삭제하는 코드입니다.
```
let re = Regex::new("[aeiou]").unwrap();
let new_string = re.replace_all(string, "");
```
먼저 `Regex::new()`를 이용하여 정규식 패턴을 지정하고, `.replace_all()` 메소드를 이용하여 패턴과 일치하는 문자를 삭제한 뒤 새로운 문자열로 반환합니다. 위의 예시에서는 `[aeiou]` 패턴을 설정하여 모음을 삭제하도록 하였습니다. 물론 해당 패턴을 다른 것으로 바꿔서 원하는 문자들을 삭제할 수도 있습니다.

## 참고 자료
- [Rust 공식 문서 - 문자열](https://doc.rust-lang.org/std/string/)
- [Rust Cookbook - 문자열 다루기](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html)
- [Rust 정규식 패턴](https://doc.rust-lang.org/regex/regex/index.html)