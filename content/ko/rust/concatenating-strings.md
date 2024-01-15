---
title:                "문자열 연결"
html_title:           "Rust: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 참여하는 이유는 단순하게 다른 문자열과 결합하여 보다 복잡한 문자열을 만들기 위해서일 수 있습니다. 이는 문자열을 조작하여 다양한 결과를 얻기 위한 중요한 기술입니다.

## 어떻게

문자열을 연결하는 것은 Rust 프로그래밍에서 매우 일반적인 작업입니다. 다음은 ```Rust``` 코드 블록 안에 예시와 결과를 포함한 코딩 예제입니다.

```
// 기본적인 문자열 연결
let str1 = "Hello";
let str2 = "world!";
let combined = format!("{} {}", str1, str2);
println!("{}", combined); // output: Hello world!

// 변수에 저장된 문자열 연결
let name = "John";
let greeting = "Hello";
let combined = format!("{} {}", greeting, name);
println!("{}", combined); // output: Hello John
```

## Deep Dive

Rust에서는 문자열을 연결하는 다양한 방법이 있으며, 가장 일반적인 방법은 ```format!``` 매크로를 사용하는 것입니다. 이 매크로는 문자열로 변환되는 것을 허용하는 모든 타입을 인자로 사용할 수 있습니다. 또한, 문자열 리터럴뿐만 아니라 동적으로 생성된 문자열을 연결하는 것도 가능합니다.

또 다른 방법으로는 ```+``` 연산자를 사용하는 것이 있습니다. 하지만 이 방법은 많은 문자열이 연결되는 경우 매크로보다는 비효율적일 수 있으니 신중하게 사용해야 합니다.

## 참고 자료

관련된 더 많은 정보를 얻기 위해 다음 링크들을 참고하세요:

- [Rust 공식 문서](https://doc.rust-lang.org/stable/std/fmt/#format)
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html)
- [Rustlings](https://github.com/rust-lang/rustlings/)