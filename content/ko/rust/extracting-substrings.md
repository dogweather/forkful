---
title:                "Rust: 부분 문자열 추출하기"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

사람들은 문자열에서 일부 부분을 추출하고 싶을 때가 있습니다. 이것은 문자열 처리의 일반적인 작업입니다. 예를 들어, 특정 단어를 찾고자 할 때, 그 단어가 포함된 문자열에서 해당 부분을 추출하게 됩니다. 또는 특정 문자열이 다른 문자열의 일부인지를 확인해야 할 때도 있습니다. 이러한 이유로, 문자열에서 일부 부분을 추출하는 기능은 프로그래머에게 매우 유용합니다.

## 어떻게?

Rust에서는 `slice` 메소드를 사용하여 문자열에서 일부 부분을 추출할 수 있습니다. 예를 들어, "Hello World"라는 문자열에서 "World" 부분만 추출하고 싶다면 다음과 같이 작성할 수 있습니다:

```Rust
let my_string = String::from("Hello World");
let world_string = &my_string[6..];
println!("{}", world_string); // 출력 결과: World
```

위 코드에서 "6.."는 index 6부터 끝까지를 뜻하며, `&` 기호는 borrowed reference를 의미합니다. 이렇게 하면 원본 문자열에 대한 ownership이 유지되면서 해당 부분을 추출할 수 있습니다.

또한, `substring` 메소드를 사용하여 시작 인덱스와 끝 인덱스를 명시적으로 지정할 수도 있습니다. 예를 들어, 위의 코드를 다음과 같이 변경할 수 있습니다:

```Rust
let my_string = String::from("Hello World");
let world_string = my_string.substring(6, 11);
println!("{}", world_string); // 출력 결과: World
```

`substring` 메소드에서는 끝 인덱스가 실제 문자열의 길이보다 크더라도 오류가 발생하지 않으며, 그 부분만 잘리게 됩니다.

## 더 깊게

Rust에서 문자열의 일부분을 추출하는 방법에는 여러 가지가 있습니다. 하지만 중요한 점은 문자열의 일부분을 추출하면 새로운 문자열이 생성되는 것이 아니라 borrowed reference가 생성된다는 것입니다. 이는 원본 문자열에 대한 ownership이 유지되면서 메모리 관리를 더욱 효율적으로 할 수 있다는 장점을 가지고 있습니다.

더 자세한 내용은 [Rust 공식 문서](https://doc.rust-lang.org/std/primitive.str.html#method.slice)를 확인하시기 바랍니다.

## 더 보기

- [Rust 버전 관리(Version Control) 시스템의 비교](https://parksb.github.io/article/28.html)
- [Rust 기초 문법 안내](https://seedjob.blog.me/221269709844)
- [Rust 공식 홈페이지](https://www.rust-lang.org/)