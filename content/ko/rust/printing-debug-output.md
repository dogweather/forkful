---
title:                "Rust: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
디버그 출력을 활용하는 이유는 무엇일까요? 디버깅을 위해 구성한 코드에 더 많은 정보를 확인하기 위해서입니다. 오류를 찾는데 도움을 주기 때문에 디버그 출력은 프로그래밍에서 매우 유용하고 반드시 필요합니다.

## 하는 법
아래 코드 블록에는 Rust 언어를 이용한 예시와 출력이 포함되어 있습니다. 해당 코드를 복사하고 컴파일해보세요!

```Rust
// 디버그 출력을 위한 println! 매크로 사용 예제
fn main() {
    let num1 = 10;
    let num2 = 5;
    let sum = num1 + num2;

    println!("{}와 {}의 합은 {}입니다.", num1, num2, sum);
    // Output: 10와 5의 합은 15입니다.
}
```

출력 결과를 보면 디버그 출력을 활용할 때 어떤 정보를 얻을 수 있는지 알 수 있습니다. 이번에는 변수의 자료형도 함께 출력해보겠습니다.

```Rust
// 자료형을 포함한 디버그 출력을 위한 format! 매크로 사용 예제
fn main() {
    let num = 10;
    let square = num * num;

    println!("{}", format!("변수 num은 {}이고, num의 제곱값은 {}입니다.", num, square));
    // Output: 변수 num은 10이고, num의 제곱값은 100입니다.
}
```

보시다시피 format! 매크로를 사용하면 더 복잡한 형식의 디버그 출력도 가능합니다. 디버그 출력을 활용할 때는 매크로와 """rust로 시작해 """로 끝나는 코드 블록을 알아두시면 매우 유용하게 활용할 수 있습니다.

## 딥 다이브
디버그 출력은 프로그래밍에서 매우 중요한 역할을 합니다. 하지만 너무 많은 디버그 출력을 사용하면 코드의 가독성이 떨어질 수 있으니 주의해야 합니다. Rust에서는 샤딩(println! 매크로 사용 시 `#[cfg]*`를 사용해 컴파일할 때 디버그 출력을 제한할 수 있는 기능을 제공합니다. 이를 통해 필요한 부분에만 디버그 출력을 적절하게 활용할 수 있습니다.

또한, Rust에서는 디버그 출력을 위해 `format!` 대신 `eprintln!` 매크로를 사용할 수도 있습니다. `eprintln!`은 오류가 발생했을 때 출력을 확인하는 것이 유용하기 때문에 실제 코드에서도 자주 사용됩니다.

## 더 알아보기
Rust에서 디버그 출력을 활용하는 방법을 자세히 알아보려면 아래 블로그 포스트를 참고해보세요!

- [Rust 디버깅 팁](https://blog.rust-lang.org/2020/01/08/rust-debugging-tools.html)
- [Rust로 디버깅하기](https://code.visualstudio.com/docs/rust/debugging)
- [Rust 'format!' 매크로 문서](https://doc.rust-lang.org/std/macro.format.html)

## 관련 링크
- [Rust 공식 문서](https://www.rust-lang.org/ko)
- [Rust 코딩 컨벤션](https://github.com/rust-lang/rfcs/blob/master/text/0259-conventions-galore.md)
- [Rust 공식 블로그](https://blog.rust-lang.org/)