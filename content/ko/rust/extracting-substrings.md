---
title:                "부분 문자열 추출하기"
html_title:           "Rust: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

이번에는 Rust 언어로 문자열의 일부분을 추출하는 방법에 대해 알아보도록 하겠습니다. 이 기능을 알게 되면 보다 효율적인 문자열 처리가 가능하며, 문제 해결 능력도 향상될 수 있습니다.

## 어떻게

우선, `str` 변수를 생성하고 그 안에 문자열을 할당합니다. 그리고 `substring()` 메소드를 사용하여 원하는 부분 문자열을 추출할 수 있습니다.

예를 들어, 다음과 같은 코드를 작성해보겠습니다.

```rust
let name = "Amy Lee"; // str 변수 생성
let last_name = name.substring(4..); // "Lee"를 추출하여 last_name 변수에 할당
println!("{}", last_name); // 콘솔에 "Lee" 출력
```

위 코드에서 `substring()` 메소드는 첫 번째 인덱스 뒤부터 끝까지의 문자열을 추출하도록 설정하였습니다. 이처럼 시작과 끝 인덱스를 지정하여 원하는 부분 문자열을 추출할 수 있습니다.

또한, 원하는 문자열을 찾기 위해 `find()` 메소드를 사용하여 해당 문자열이 시작하는 인덱스를 찾은 후 `substring()` 메소드를 통해 추출할 수도 있습니다.

```rust
let sentence = "I love Rust language";
let love_index = sentence.find("love").unwrap(); // "love"가 시작하는 인덱스를 찾은 후 love_index 변수에 할당
let rust = sentence.substring(love_index + 5..); // love_index 다음부터 끝까지 추출하여 rust 변수에 할당
println!("{}", rust); // 콘솔에 "Rust language" 출력
```

여기서 `unwrap()` 메소드는 `find()` 메소드가 찾는 문자열을 찾지 못한 경우 오류를 표시하지 않고 `None`이나 `Err`값을 반환하는 것을 방지하기 위해 사용됩니다.

## 깊게 파고들기

위의 예시에서는 `substring()` 메소드를 사용하여 부분 문자열을 추출했지만, Rust에서는 문자열 슬라이싱(slice)을 사용하여 같은 작업을 수행할 수도 있습니다.

문자열 슬라이싱은 `[]` 안에 시작과 끝 인덱스를 적어 원하는 부분 문자열을 추출하는 방식입니다. 선언한 문자열 변수 뒤에 `[]`을 붙여 인덱스를 지정하면, 해당하는 부분 문자열이 추출되게 됩니다.

```rust
let song = "Bring Me to Life";
println!("{}", song[11..]); // 콘솔에 "Life" 출력
```

또한, `chars()` 메소드를 사용하여 각 문자를 분리해낼 수도 있으며, 이를 활용하여 특정 문자열을 제거하는 등의 작업도 가능합니다.

```rust
let sentence = "Hello, my name is Jin";
let jin_name = sentence.chars().skip(17).collect::<String>(); // "my name is Jin" 추출
println!("{}", jin_name); // 콘솔에 "my name is Jin" 출력
```

## 참고 자료

- [Rust언어 공식 홈페이지](https://www.rust-lang.org/ko/)
- [Rust언어 한국 커뮤니티](https://www.rustlang.kr/)
- [Rust언어 공식 도큐먼트](https://doc.rust-lang.org/std/string/struct.String.html)