---
date: 2024-01-20 17:51:06.554746-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04(string interpolation)\uC740 \uBCC0\uC218\
  \uB098 \uD45C\uD604\uC2DD\uC744 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uB07C\uC6CC\uB123\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uB370\uC774\uD130\uB97C \uB3D9\uC801\uC73C\uB85C\
  \ \uBB38\uC790\uC5F4\uACFC \uD569\uCCD0\uC11C \uC77D\uAE30 \uC26C\uC6B4 \uACB0\uACFC\
  \uB97C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.063233
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04(string interpolation)\uC740 \uBCC0\uC218\
  \uB098 \uD45C\uD604\uC2DD\uC744 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uB07C\uC6CC\uB123\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uB370\uC774\uD130\uB97C \uB3D9\uC801\uC73C\uB85C\
  \ \uBB38\uC790\uC5F4\uACFC \uD569\uCCD0\uC11C \uC77D\uAE30 \uC26C\uC6B4 \uACB0\uACFC\
  \uB97C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
문자열 보간(string interpolation)은 변수나 표현식을 문자열 안에 끼워넣는 것입니다. 데이터를 동적으로 문자열과 합쳐서 읽기 쉬운 결과를 만들기 위해 사용합니다.

## How to: (어떻게 하나요?)
```kotlin
fun main() {
    val name = "준호"
    val age = 28
    // 문자열 보간 사용
    println("안녕하세요, 제 이름은 $name이고, 나이는 $age살 입니다.")
    // 표현식도 사용 가능
    println("내년이면 ${age + 1}살이 되겠군요!")
}
```

출력 결과:
```
안녕하세요, 제 이름은 준호이고, 나이는 28살 입니다.
내년이면 29살이 되겠군요!
```

## Deep Dive (깊이 알아보기)
코틀린에서 문자열 보간은 `$` 기호를 사용합니다. 이 기능은 Java에선 `+` 연산자나 `String.format` 메서드로 문자열에 데이터를 추가하는 것보다 편리하고 가독성이 좋습니다. 내부적으로 코틀린은 `toString()` 메서드를 호출하여 해당 변수나 표현식의 문자열 표현을 생성한 후 최종 문자열을 조합합니다. 복잡한 표현식의 경우 중괄호 `{}`로 감싸서 사용합니다.

## See Also (더 알아보기)
- 코틀린 공식 문서: [Basic Types](https://kotlinlang.org/docs/basic-types.html#string-templates)
