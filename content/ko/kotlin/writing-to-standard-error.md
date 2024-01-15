---
title:                "표준 오류에 쓰기"
html_title:           "Kotlin: 표준 오류에 쓰기"
simple_title:         "표준 오류에 쓰기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
"Standard error"에 대해 작성하는 것의 이유는 프로그램을 디버그하거나 문제를 해결하기 위해서입니다.

## 방법
```
Kotlin System.err.println("Hello, World!")
```
위의 코드는 "Hello, World!"를 "Standard error"에 출력하는 코틀린의 기본적인 예시입니다.

```
Kotlin val x: Int? = null
System.err.println(x!!)
```
위의 코드는 Nullable 변수에 값이 없을 때 "Standard error"에 예외를 발생시키는 예시입니다.

## 깊이 살펴보기
"Standard error"는 프로그래머에게 디버깅과 예외 처리를 위한 매우 유용한 도구입니다. 코틀린에서는 "System.err"을 사용하여 "Standard error"로 메시지를 보낼 수 있으며, 이는 프로그램의 실행 중에 발생한 오류를 살펴볼 수 있게 해줍니다. 또한, "Standard error"는 기존에 출력된 메시지들과 구분되기 때문에 프로그램의 오류를 식별하기 쉬워집니다.

## 참고
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#nullable-types)
- [Standard error와 Standard out의 차이](https://stackoverflow.com/questions/4697432/what-are-the-differences-between-stderr-and-stdout)
- [코틀린 기본 문법](https://kotlinlang.org/docs/reference/basic-syntax.html)