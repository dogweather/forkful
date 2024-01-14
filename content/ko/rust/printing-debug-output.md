---
title:    "Rust: 디버그 출력 출력하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

목적: 디버그 출력을 찍는 것의 *왜* 중요한지에 대해서 설명한다.

디버그 출력은 프로그래밍 과정에서 매우 중요한 역할을 담당합니다. 코드가 작동하는 과정에서 문제가 발생할 때, 디버그 출력을 통해 어떤 문제가 발생하고 있는지 쉽게 파악할 수 있습니다. 또한, 디버그 출력을 이용하면 코드의 흐름을 추적하고 변수의 값을 확인하는 데에도 도움이 됩니다.

어떻게: 코딩 예제와 함께 "```Rust ... ```" 코드 블록 안에서 샘플 출력을 제공합니다.

```rust
// 디버그 출력 예제
let name = "John";
let age = 25;
println!("이름: {}, 나이: {}", name, age); // 새로운 줄로 출력됨
print!("이름: {}, 나이: {}", name, age); // 같은 줄에 출력됨
```

출력 결과:
```
이름: John, 나이: 25
이름: John, 나이: 25
```

깊게 들어가기: 디버그 출력에 대해 더 깊이 알아보겠습니다. 먼저, `println!`과 `print!`는 형식 문자열을 사용하며, `println!`은 출력 후 새로운 줄로 넘어가고, `print!`는 같은 줄에 출력합니다. 또한, 형식 문자열에는 변수나 값을 넣는 데 사용되는 형식 지정자가 있습니다.

예를 들어, `%d`는 10진수 정수를, `%.2f`는 소수점 두 자리까지 반올림한 실수를 출력합니다. 이 외에도 여러 형식 지정자를 사용할 수 있으며, 필요한 경우 문자열의 형식을 정확하게 지정해야 합니다.

### 추천 링크
- [Rust 공식 문서- 디버그 출력](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
- [Rust 디버깅하기 - TMI를 활용한 디버깅 출력](https://blog.mozilla.org/research/2015/05/11/rust-debugging-with-tmi/)
- [The Art of Rust - 디버깅 팁](https://medium.com/@kevin_lynx/the-art-of-rust-debugging-tips-cae54cdcd9e)