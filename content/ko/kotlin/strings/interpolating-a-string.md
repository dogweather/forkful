---
title:                "문자열 보간하기"
aliases:
- /ko/kotlin/interpolating-a-string/
date:                  2024-01-20T17:51:06.554746-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/interpolating-a-string.md"
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
