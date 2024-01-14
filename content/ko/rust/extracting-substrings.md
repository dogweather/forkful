---
title:                "Rust: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

때로는 문자열에서 일부분만을 추출해야 할 때가 있습니다. 이를 위해 Rust의 문자열 추출 기능을 활용할 수 있습니다. 이 기능을 사용하면 더 나은 코드를 작성하고 문자열을 효율적으로 다룰 수 있습니다.

## 사용 방법

추출하려는 문자열이 있는 변수를 만들고, `substring` 함수를 사용하여 시작 위치와 길이를 지정할 수 있습니다. 이 함수는 해당 문자열의 일부분만 복사하여 새로운 문자열 객체를 반환합니다. 아래는 예시 코드와 출력 결과입니다.

```Rust
let word = "Rust 프로그래밍";
let extracted = word.substring(5, 6);
println!("{}", extracted);
```

```
프
```

## 깊이 파고들기

`substring` 함수를 사용하여 추출된 문자열은 지정된 길이 이상일 때 오류가 발생하지 않습니다. 대신, 최대 길이까지만 추출되며 나머지는 무시됩니다. 또한, 문자열의 시작 위치와 끝 위치가 각각 0 또는 음수일 때는 문자열의 오른쪽 끝부터 시작 위치를 계산합니다. 예를 들어, `substring(0, 3)`이나 `substring(-4, 3)`의 경우, 문자열의 첫 3글자를 추출하게 됩니다.

## 참고 자료

- [Rust 공식 문서 - 문자열 자르기](https://doc.rust-lang.org/std/primitive.str.html#method.substring)
- [Rust book - 텍스트 처리 - 문자열 슬라이스](https://doc.rust-lang.org/book/ch08-03-hash-maps.html#summary)

## 더 알아보기

Rust 문자열 추출 기능을 사용하는 방법에 대해 더 자세히 알아보고 싶다면, 위의 참고 자료를 확인해 보세요. 또한, Rust의 다른 문자열 관련 메소드에 대해서도 학습할 수 있습니다.