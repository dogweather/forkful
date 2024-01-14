---
title:                "Rust: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 자주 사용되는 *정규 표현식*에 대해 살펴보는 것이 좋을까요?
정규 표현식은 문자열에서 특정한 패턴을 찾거나 대체하려는 경우 매우 유용합니다. Rust에서도 많은 유저들이 이를 활용하면서 프로그래밍 작업을 효율적으로 할 수 있게 되었습니다.

## 어떻게 사용할까요?
여러분의 Rust 코드 안에서 정규 표현식을 사용하는 방법을 알아보겠습니다. 먼저, `regex` 패키지를 다운로드 하세요.

```Rust
use regex::Regex; // regex 패키지 불러오기
```

이제, 패턴을 포함하는 정규 표현식을 정의할 수 있습니다.

```Rust
let re = Regex::new(r"hello"); // "hello" 패턴을 가진 정규 표현식 정의
```

이제, 이 정규 표현식을 이용해 특정 문자열에 해당 패턴이 있는지 확인할 수 있습니다.

```Rust
let result = re.is_match("hello world"); // "hello world"에 "hello" 패턴이 있는지 확인
println!("{:?}", result); // true가 출력됩니다.
```

## 깊이있게 알아보기
정규 표현식은 더 복잡한 패턴을 찾기 위해 다양한 메타 문자와 제어 문자를 사용할 수 있습니다. 예를 들어, `+`는 이전 패턴이 한번 이상 반복되는 경우에 사용할 수 있습니다. `*`는 이전 패턴이 0번 이상 반복되는 경우에 사용합니다. `?`는 이전 패턴이 있어도 되고 없어도 되는 경우에 사용합니다.

```Rust
let re = Regex::new(r"[0-9]+"); // 숫자가 한 번 이상 반복되는 경우를 찾는 정규 표현식 정의
```

이외에도, `|`를 사용해 여러 개의 패턴 중 하나를 찾을 수 있으며, `()`를 사용해 그룹을 지정할 수 있습니다.

## 더 읽어보기
- Rust 정식 문서의 [Regex 모듈](https://doc.rust-lang.org/regex/regex/index.html) 소개 페이지
- Rust 프로그래밍 언어에 대한 [정규 표현식 사용법](https://doc.rust-lang.org/stable/rust-by-example/std/regex.html) 예제
- [정규 표현식 온라인 테스트 도구](https://regex101.com/)로 직접 실험해보기

## 참고 자료
- [Rust Cookbook 중 정규 표현식](https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html)
- [정규 표현식을 활용한 문자열 처리](https://jungledot.net/post/be/iiomR6pV