---
title:                "Gleam: 대소문자 변환하기"
simple_title:         "대소문자 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 왜 필요한지에 대해 이야기해보겠습니다. 문자열은 프로그래밍에서 매우 중요한 데이터 유형 중 하나입니다. 때로는 이를 다른 메소드와 함께 사용해야 할 때가 있습니다. 이러한 경우 문자열을 소문자로 변환해야 할 수도 있습니다. 그러면 왜 이렇게 해야하는지 알아봅시다.

## 하우 투

본격적으로 문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다. 이 작업은 Gleam에서 아주 간단합니다. 우선, 소문자로 변환하려는 문자열을 지정한 다음 `String.to_lower_case()` 메소드를 사용하면 됩니다. 아래의 코드 예제를 참조하세요.

```Gleam
let str = "Hello World"
let lower = string.to_lower_case(str)
```

위의 코드는 "Hello World"라는 문자열을 소문자로 변환한 후 `lower` 변수에 저장하는 예제입니다. `lower` 값을 출력하면 `hello world`라는 결과를 얻게 됩니다.

## 딥 다이브

이제 우리는 문자열을 소문자로 변환하는 방법을 배웠지만, 그것이 어떻게 작동하는지 더 깊이 이해하고 싶을 수도 있습니다. Gleam에서는 모든 문자열을 소문자로 바꾸는 메소드가 내장되어 있기 때문에 별도의 로직이 필요하지 않습니다. 이렇게 하기 위해서는 다른 프로그래밍 언어에서와 같이 반복문이나 조건문을 사용할 필요가 없습니다. Gleam은 이 작업을 더 효율적으로 처리할 수 있도록 설계되었습니다.

## 같이 보기

이제 문자열을 소문자로 변환하는 방법을 배웠으니 다른 관련 자료를 살펴보세요.

- [`String.to_upper_case()` 메소드](https://gleam.run/documentation/current/std/string.html#to_upper_case)
- [Gleam 문자열 관련 내장 함수](https://gleam.run/documentation/current/std/string.html)