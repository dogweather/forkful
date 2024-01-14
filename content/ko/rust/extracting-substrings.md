---
title:    "Rust: 부분 문자열 추출"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜
Rust 프로그래밍을 배우고 있다면, 문자열을 다루는 것은 피할 수 없는 부분입니다. 이번 블로그 포스트에서는 Rust에서 문자열을 추출하는 방법에 대해 알아보겠습니다.

## 추출하는 방법
문자열에서 부분 문자열을 추출하는 방법은 간단합니다. 우선, 다음과 같은 문자열 변수가 있다고 가정해봅시다.

```Rust
let string = "Hello, Rust!";
```

만약, "Hello"라는 부분 문자열을 추출하고 싶다면, `&` 연산자와 함께 `[]`를 사용하여 다음과 같이 작성할 수 있습니다.

```Rust
let extracted = &string[0..5];
println!("{}", extracted);
```

출력 결과는 "Hello"가 될 것입니다. `[]` 안에는 시작 인덱스와 마지막 인덱스를 지정해줘야 하는데, 마지막 인덱스는 추출하는 부분 문자열의 다음 인덱스를 가리켜야 합니다.

또한, 인덱스를 지정할 때는 byte의 길이를 기준으로 해야 합니다. 예를 들어, "안녕하세요"라는 문자열이 있다면, 인덱스는 한글이 3byte라는 점을 고려하여 지정해주어야 합니다. 이렇게 부분 문자열을 추출하는 방법을 익히면, 보다 다양한 문자열 조작을 할 수 있게 됩니다.

## 깊이 파고들기
위에서 설명한 추출하는 방법은 간단하면서도 유용합니다. 하지만, 실제로는 더 많은 것들을 고려해야 할 때가 있습니다. 예를 들어, 문자열의 글자 수를 초과하여 인덱스를 지정하면 Rust는 에러를 발생시킵니다. 이러한 에러를 처리하려면, `get()` 함수를 사용하여 다음과 같이 작성할 수 있습니다.

```Rust
if let Some(extracted) = string.get(0..5) {
    println!("{}", extracted);
} else {
    println!("String too long!");
}
```

이렇게 하면, 인덱스 초과 에러를 처리할 수 있습니다.

## 또 다른 정보 알아보기
추출하는 방법에 대해 더 알고 싶다면, [Rust 공식 문서](https://doc.rust-lang.org/std/primitive.str.html#method.slice)를 참고해보세요. 또한, [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)에서도 더 많은 예제를 볼 수 있습니다.

## 또 보기
[Rust로 문자열 조작하기 - 문자열 탐색](https://tech.io/playgrounds/3490/rust---string-manipulation/searching-a-string)