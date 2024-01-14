---
title:    "Rust: 문자열 대문자로 변환하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜 문자열 대문자로 변경하는가?

최근 프로그래밍 언어로서 인기를 얻고 있는 러스트(Rust)는 높은 성능과 안정성으로 유명하다. 그런데 여러분은 이미 존재하는 문자열을 대문자로 변경하는 방법이 궁금할 수 있다. 이번 글에서는 러스트를 사용하여 문자열을 쉽게 대문자로 변경하는 방법을 알려드리겠다.

## 방법

문자열을 대문자로 변경하는 가장 간단한 방법은 `to_uppercase()` 메서드를 사용하는 것이다. 이 메서드는 `String` 타입에서 사용 가능하며, 문자열을 대문자로 변경한 결과를 새로운 `String` 타입으로 리턴한다.

```Rust
let s = "hello";
let result = s.to_uppercase();

println!("{}", result); // 출력 결과: HELLO
```

`let result = s.to_uppercase();` 코드를 보면, 메서드를 호출하는 소유자는 `s` 변수이고, 이는 `String` 타입이기 때문이다. 따라서 `to_uppercase()` 메서드를 사용할 때에는 반드시 `String` 타입을 가진 변수에서 호출해야 한다.

만약 `String` 타입이 아닌 `&str` 타입의 문자열에서 대문자로 변경하고 싶다면 어떻게 할까? 이때는 `to_uppercase()`를 사용하는 대신 `to_uppercase()` 메서드를 포함하는 `String` 타입을 만들어야 한다. 이렇게 하면 문자열이 `String` 타입으로 바뀌고 대문자로 변경할 수 있다.

```Rust
let s = "hello";
let result = s.to_uppercase().to_string();

println!("{}", result); // 출력 결과: HELLO
```

## 깊게 파보기

위에서 살펴본 `to_uppercase()` 메서드는 매우 간단하지만, `String` 타입에서 상당히 많이 사용되는 메서드이다. 그래서 러스트에서는 이 메서드를 최적화하여 더 효율적으로 동작하게 만들었다. `to_uppercase()` 메서드는 각 문자의 대소문자 여부를 판별하는 대신, 그저 영문의 알파벳 범위 내에서 소문자를 찾아 대문자로 변경하도록 동작한다. 이렇게 하면 처리 속도가 매우 빨라지기 때문에, 대량의 문자열을 변경하거나 반복적으로 `to_uppercase()`를 사용하는 경우에는 더욱 뛰어난 성능을 발휘한다.

## 관련 링크

- [러스트 공식 문서 - 문자열 메서드](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [러스트 공식 가이드 - 자주 사용하는 문자열 처리 메서드](https://rust-lang.github.io/rust-cookbook/text/strings.html#commonly-used-methods)