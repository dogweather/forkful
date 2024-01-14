---
title:    "Rust: 문자열을 소문자로 변환하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 것에 대해 관심이 생기는 이유는 다양할 수 있습니다. 예를 들어, 대소문자를 구분하지 않고 문자열을 비교해야 할 때, 또는 특정 작업을 수행하기 위해 문자열의 양식을 통일해야 할 때 등이 있을 수 있습니다. 또는 간단히 문자열을 보기 좋게 표현하기 위해서도 사용될 수 있습니다. Rust는 이를 위한 간단하고 효율적인 방법을 제공하고 있으니 이번 글에서는 그 방법에 대해 알아보도록 하겠습니다.

## 어떻게

Rust에서 문자열을 소문자로 변환하기 위해서는 `to_lowercase()` 함수를 사용하면 됩니다. 이 함수는 `String` 타입의 문자열을 반환하며, 인자로는 변환하고자 하는 문자열의 참조자를 전달해주어야 합니다.

```Rust
fn main() {
    let name = "RUST PROGRAMMING";
    let lower_name = name.to_lowercase();
    println!("{}", lower_name);
}
```

위의 코드에서 `to_lowercase()` 함수를 이용해 `name` 변수에 할당된 문자열을 소문자로 변환하여 `lower_name` 변수에 저장했습니다. 그리고 `println!` 매크로를 이용해 변환된 소문자 문자열을 출력하도록 하였습니다. 이를 실행하면 다음과 같은 결과가 나타납니다.

```
rust programming
```

이와 같이 소문자로 변환된 결과를 얻을 수 있습니다.

## 깊게 들어가기

Rust에서 문자열을 소문자로 변환하는 방법은 간단하지만, 실제로는 다소 복잡한 과정을 거칩니다. 특히, 영어 이외의 언어에서는 더욱 복잡한 경우가 있을 수 있습니다. Rust는 이를 해결하기 위해 내부적으로 `Unicode`와 관련된 기능을 활용하여 문자열을 변환합니다.

예를 들어, 여러 언어에서 공백이나 기호 등을 포함한 문자열을 소문자로 변환해야 할 때, 이러한 문자들을 각각 모두 소문자로 변환하는 것이 아니라 해당 문자열의 `Unicode`를 고려하면서 정확하게 변환해줍니다. 또한, 다양한 언어 패턴과 규칙에 대해 자세히 알고 있어야만 정확한 변환 결과를 얻을 수 있습니다.

따라서 Rust에서 문자열을 소문자로 변환할 때는 이러한 내부적인 과정을 고려해야 한다는 점을 명심하시길 바랍니다.

# 관련 링크

- [Rust 문자열 관련 공식 문서](https://doc.rust-lang.org/beta/std/string/struct.String.html#method.to_lowercase)
- [Rust 문자열 타입과 메서드 소개](https://www.rprogramming.net/rust-string-tutorial/)
- [Rust 표준 라이브러리](https://doc.rust-lang.org/std/index.html)