---
title:    "Rust: 문자열 연결하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜 이러한 내용을 다루는 것인가?

문자열을 이어 붙이는 것은 모든 프로그래밍 언어에서 자주 사용되는 기술입니다. 예를 들어, 문자열로 구성된 이름과 성 같은 정보를 결합하거나 다양한 사용자 정보를 표시하기 위해 사용됩니다. Rust에서는 문자열을 옵션으로 사용하거나 String 버퍼를 사용하여 문자열을 이어 붙일 수 있습니다. 이번 글에서는 기본적인 방법이 아니라 Reallocating string을 사용하여 문자열을 이어 붙이는 방법을 살펴보겠습니다.

## 어떻게 이어 붙이나요?

문자열을 이어 붙이는 가장 기본적인 방법은 "format!" 매크로를 사용하는 것입니다. 이를 위해서는 std::fmt::Argument를 먼저 정의해야 합니다. 예를 들어, 우리는 문자열 표기 방식으로 이름과 성을 받아서 새로운 문자열을 리턴하는 함수를 만들 수 있습니다.

```rust
use std:fmt;

fn full_name(first_name: &str, last_name: &str) -> String {
    let name = format!("{} {}", first_name, last_name);
    return name;
}

fn main() {
    let name = full_name("John", "Smith");
    println!("{}", name);
}
```

위의 예제에서 "John Smith"가 출력됩니다. format! 매크로는 String 버퍼를 자동으로 만들어줍니다. 코드 블록 밖에서 컴파일러는 String의 내용을 복사하기 위해 이 버퍼에 근거합니다. 이를 통해 문자열을 이어 붙일 때마다 String의 길이를 재할당할 필요가 없어집니다. 이러한 방식은 간단하고 효율적이지만, 항상 재할당이 발생한다는 단점이 있습니다.

다음으로는 String의 capacity를 직접 지정하는 방법입니다. 이를 통해 재할당이 더 적게 발생하여 성능을 향상시킬 수 있습니다. 예를 들어, 만약 무언가 긴 문자열을 합쳐야 할 때, capacity를 크게 설정하여 필요한 재할당을 최소화할 수 있습니다.

```rust
use std::string::String;

fn main() {
    let mut string1 = String::from("Hello, ");
    string1.reserve(20);
    string1.push_str("world!");
    println!("{}", string1);
}
```

위의 예제에서 String의 capacity를 미리 설정하고, push_str() 함수를 통해 새로운 문자열을 추가합니다. 이 방식은 더 많은 메모리를 사용하여 재할당을 최소화하지만, 여전히 String 버퍼를 새로운 String으로 복사해야 한다는 단점이 있습니다. 

## 더 깊게 들어가기

이어 붙이기의 세 번째 방법은 Reallocating string이라고 불리는 것입니다. 이것은 문자열을 추가하기 전에 capacity를 검증하고 필요한 경우 capacity를 늘리는 방식입니다. 이를테면 이어 붙일 문자열이 기존 capacity보다 길 경우, capacity를 늘리는 것입니다. 이 방식을 구현하기 위해서는 기존 String의 내용을 복사하지 않고, 우리가 이어 붙일 문자열의 capacity를 검증하여 필요하면 늘리는 과정을 거쳐야 합니다. 

Reallocating string을 구현하는 방법은 여러 가지가 있습니다. 가장 간단한 방법은 String의 .with_capacity(n) 메서드를 사용하여 미리