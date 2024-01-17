---
title:                "문자열 연결하기"
html_title:           "Rust: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 연결(concatenating)이란 무엇일까요? 간단하게 말하자면, 두 개 이상의 문자열을 하나로 이어 붙이는 것을 의미합니다. 프로그래머들은 문자열을 연결하는 이유는 다양합니다. 예를 들어, 여러 개의 변수를 하나의 큰 문자열로 만들어서 특정 작업을 수행하기 위해서일 수도 있고, 두 개의 문자열을 결합하여 새로운 문자열을 만들어서 사용자에게 출력하는 등의 목적이 있을 수 있습니다.

## 하는 방법:
```Rust
let string1 = "Hello, ";
let string2 = "world!";
let result = string1.to_string() + string2;
println!("{}", result);
```
출력 결과:
```
Hello, world!
```

```Rust
let a = "Rust";
let b = " programming";
let c = " language";
let result = format!("{} {}{}", a, b, c);
println!("{}", result);
```
출력 결과:
```
Rust programming language
```

## 더 들어가기:
문자열의 연결 역사를 살펴보면, 예전에는 문자열 연결을 위해 여러가지 방법이 사용되었습니다. 예를 들어, C 언어에서는 strcat 함수를 사용했는데, 이는 두 개의 문자열을 인자로 받아서 첫 번째 문자열에 두 번째 문자열을 이어 붙이는 기능을 수행했습니다. 그러나 이 방법은 보안 취약점을 가지고 있기 때문에, 안전한 문자열 연결 방법인 Rust의 String 타입과 관련 메소드들이 생겨났습니다. 또한, 문자열을 연결하는 다른 방법으로는 빌더 패턴이 있습니다. 이 방법은 여러 개의 문자열을 이어붙이는 것보다 메모리를 더 효율적으로 사용할 수 있지만, 코드가 복잡해지는 단점이 있습니다.

## 관련 자료:
- [Rust 공식 문서 - 문자열](https://doc.rust-lang.org/std/string/)
- [Rust By Example - 문자열 연결](https://doc.rust-lang.org/stable/rust-by-example/std/strings.html)
- [Rust String 타입에 대한 더 자세한 알아보기](https://medium.com/@kh7070821/rust-string-%ED%83%80%EC%9E%85%EC%97%90-%EB%8C%80%ED%95%9C-%EB%8D%B0%EC%9D%B4%ED%84%B0-%EC%89%BD%EA%B2%8C-%EC%8B%9C%EC%9E%91%ED%95%98%EA%B8%B0-f6d36601c90e)