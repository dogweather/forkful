---
title:                "Rust: 문자열의 길이 찾기"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

Rust 프로그래밍은 안전하고 효율적인 언어로서 다양한 분야에서 사용되고 있습니다. 문자열을 다루는데 있어서도 Rust는 매우 강력한 기능을 제공하며, 숫자와 마찬가지로 문자열의 길이도 쉽게 찾을 수 있습니다. 이 글에서는 Rust를 사용하여 문자열의 길이를 찾는 방법을 살펴보겠습니다.

## 어떻게

먼저, `len()` 메서드를 사용해서 문자열의 길이를 찾을 수 있습니다. 다음은 Rust로 작성된 예제 코드와 출력 결과입니다.

```Rust
let string = "안녕하세요";
println!("문자열의 길이: {}", string.len());
```

출력 결과:

`문자열의 길이: 5`

`len()` 메서드는 문자열의 모든 문자를 고려하여 길이를 찾기 때문에 정확한 결과를 반환합니다. 또 다른 방법은 `chars()` 메서드를 사용해서 이터레이터로 변환한 다음, `count()` 메서드를 호출하여 길이를 찾는 것입니다.

```Rust
let string = "안녕하세요";
println!("문자열의 길이: {}", string.chars().count());
```

출력 결과:

`문자열의 길이: 5`

`count()` 메서드는 이터레이터의 요소 수를 반환하기 때문에 문자열의 길이를 찾는데 사용할 수 있습니다. 또는 `bytes()` 메서드를 사용하여 바이트 수를 카운트할 수도 있습니다.

```Rust
let string = "안녕하세요";
println!("문자열의 길이: {}", string.bytes().count());
```

출력 결과:

`문자열의 길이: 10`

바이트 수를 카운트하는 방법은 UTF-8 문자열에서는 가장 일반적인 방법입니다.

## 딥 다이브

Rust에서 문자열의 길이를 찾는 `len()` 메서드는 문자열 슬라이스로 호출할 수 있으며, `len()` 메서드는 이를 호출하는 `String` 타입에 대해서도 구현되어 있습니다. 내부적으로 `len()` 메서드는 `str` 타입의 `len()` 메서드를 호출하는 것과 동일합니다. 따라서 `str` 타입에서는 이터레이터가 없기 때문에 `len()` 메서드를 호출하여 길이를 찾거나, 바이트 수를 카운트할 수 없습니다.

그리고 `len()` 메서드의 시간 복잡도는 *O(1)*이기 때문에 매우 효율적으로 동작합니다. 이는 문자열의 길이를 찾는데 선형 시간이 소요되지 않는다는 것을 의미합니다. 따라서 Rust를 사용하여 매우 긴 문자열도 빠르게 처리할 수 있습니다.

## See Also

- [공식 Rust 문서](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Rust 프로그래밍 언어](https://www.rust-lang.org/)

이제 Rust를 사용하여 문자열의 길이를 찾는 방법에 대해 알게 되었습니다. 다음 링크를 통해 더 많은 Rust 관련 정보를 찾아보세요.