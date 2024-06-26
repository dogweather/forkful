---
date: 2024-01-20 17:51:06.554746-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCF54\uD2C0\uB9B0\uC5D0\
  \uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 `$` \uAE30\uD638\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740 Java\uC5D0\uC120 `+` \uC5F0\uC0B0\uC790\
  \uB098 `String.format` \uBA54\uC11C\uB4DC\uB85C \uBB38\uC790\uC5F4\uC5D0 \uB370\uC774\
  \uD130\uB97C \uCD94\uAC00\uD558\uB294 \uAC83\uBCF4\uB2E4 \uD3B8\uB9AC\uD558\uACE0\
  \ \uAC00\uB3C5\uC131\uC774 \uC88B\uC2B5\uB2C8\uB2E4. \uB0B4\uBD80\uC801\uC73C\uB85C\
  \ \uCF54\uD2C0\uB9B0\uC740 `toString()` \uBA54\uC11C\uB4DC\uB97C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.508926-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCF54\uD2C0\uB9B0\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 `$` \uAE30\uD638\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

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
