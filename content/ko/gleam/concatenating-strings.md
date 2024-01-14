---
title:    "Gleam: 문자열 연결하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 일이 얼마나 중요한지 궁금하지 않으세요? 만약 동적인 데이터를 처리하는 프로그램을 작성하거나, 출력할 내용을 사용자 지정하고 싶다면 문자열 연결(concatenation)은 필수적입니다!

## 어떻게

문자열을 연결하는 가장 간단한 방법은 "+" 연산자를 사용하는 것입니다:

```Gleam
let greeting = "Hello "
let name = "World"
let message = greeting + name
```

이 코드를 실행하면 `message` 변수에는 `"Hello World"`라는 새로운 문자열이 할당됩니다.

문자열을 이어 붙일 때는 빈 칸과 같은 구분자를 추가하는 것도 쉽습니다:

```Gleam
let fruits = ["apple", "orange", "banana"]
let list_of_fruits = String.join(fruits, ", ")
```

이 코드를 실행하면 `list_of_fruits` 변수에는 `"apple, orange, banana"`라는 문자열이 할당됩니다.

## 깊이 들어가기

먼저, `"++"`와 같은 다른 연산자를 사용해도 두 문자열을 연결할 수 있지만, 성능면에서는 "+" 연산자가 더 유리합니다. 이유는 `"++"` 연산자는 새로운 문자열 객체를 생성하기 때문입니다.

또한, 문자열 연결은 항상 처음부터 끝까지 문자열을 모두 새로운 메모리 공간에 할당하는 것이 아니라, 필요에 따라 새로운 공간을 할당하기 때문에 더 효율적입니다.

마지막으로, Gleam에서는 문자열 연결 시 O(1)의 시간 복잡도를 보장합니다. 따라서 더 많은 문자열을 연결하면 메모리 할당 및 성능과 관련된 문제에 직면할 수 있습니다.

## 더 보기

[Gleam 공식 문서](https://gleam.run/install/)에서 Gleam에 대해 더 알아보세요.
[Gleam Github](https://github.com/gleam-lang/gleam)에서 Gleam 소스 코드를 확인할 수 있습니다.
[Gleam 튜토리얼](https://gleam.run/book/overview.html)에서 Gleam을 배우세요.