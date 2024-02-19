---
aliases:
- /ko/kotlin/removing-quotes-from-a-string/
date: 2024-01-26 03:41:27.830545-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uB2E8\uC77C(' ') \uB610\uB294 \uC774\uC911(\" \"\
  ) \uB530\uC634\uD45C \uBB38\uC790\uC758 \uBAA8\uB4E0 \uC778\uC2A4\uD134\uC2A4\uB97C\
  \ \uCC98\uB9AC \uC911\uC778 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uC5D0\uC11C \uC81C\
  \uAC70\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uCD94\uAC00 \uCC98\uB9AC\
  \uB97C \uC704\uD55C \uC900\uBE44, \uB610\uB294 \uB530\uC634\uD45C \uC790\uCCB4\uAC00\
  \ \uB370\uC774\uD130\uC758 \uC758\uBBF8\uC640 \uAD00\uB828\uC774 \uC5C6\uC744 \uB54C\
  \ \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC\u2026"
lastmod: 2024-02-18 23:09:06.131505
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uB2E8\uC77C(' ') \uB610\uB294 \uC774\uC911(\" \") \uB530\
  \uC634\uD45C \uBB38\uC790\uC758 \uBAA8\uB4E0 \uC778\uC2A4\uD134\uC2A4\uB97C \uCC98\
  \uB9AC \uC911\uC778 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uC5D0\uC11C \uC81C\uAC70\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uCD94\uAC00 \uCC98\uB9AC\uB97C\
  \ \uC704\uD55C \uC900\uBE44, \uB610\uB294 \uB530\uC634\uD45C \uC790\uCCB4\uAC00\
  \ \uB370\uC774\uD130\uC758 \uC758\uBBF8\uC640 \uAD00\uB828\uC774 \uC5C6\uC744 \uB54C\
  \ \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 따옴표를 제거한다는 것은 단일(' ') 또는 이중(" ") 따옴표 문자의 모든 인스턴스를 처리 중인 텍스트 데이터에서 제거하는 것을 의미합니다. 프로그래머들은 데이터 정제, 추가 처리를 위한 준비, 또는 따옴표 자체가 데이터의 의미와 관련이 없을 때 이 작업을 자주 수행해야 합니다.

## 방법:

Kotlin에서 두 종류의 따옴표를 모두 제거하는 간단한 방법은 다음과 같습니다:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // 출력: Kotlin rocks its cool
}
```

그리고 한 종류의 따옴표만 제거하고 싶다면, 다른 replace 호출을 생략하면 됩니다.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // 출력: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // 출력: Kotlin "rocks" its cool
}
```

## 심화 학습

역사적으로 문자열 처리와 이스케이핑 문자는 프로그래밍의 핵심 부분이었습니다. 왜냐하면 텍스트는 데이터와 상호 작용하는 기본적인 방법이기 때문입니다. 문자열 내의 따옴표는 때때로 이스케이프되어야 합니다. 이는 선행 백슬래시에 의해 나타납니다(예: `"She said, \"Hi!\""`). 이러한 문자열을 처리할 때, 보다 깨끗하거나 사용하기 쉬운 텍스트를 위해 이스케이프 문자나 따옴표 자체를 제거해야 할 수 있습니다.

`replace` 메소드에 대한 대안으로는 정규 표현식 기반 제거나 수동으로 문자에 따라 문자열을 파싱하는 방법이 있습니다. 그러나 정규 표현식은 간단한 연산에는 과도할 수 있으며, 수동 파싱은 내장된 문자열 함수를 사용하는 것보다 효율이 떨어집니다. Kotlin의 `replace` 함수는 성능에 있어 잘 최적화된 Java의 `String` `replace` 메소드를 활용합니다.

구현 측면에서 Kotlin은 Java와 상호 운용 가능하다는 점을 언급할 가치가 있으므로, 실제로 문자열에 수행하는 모든 연산은 Java에서처럼 성능이 좋습니다. 따옴표를 제거할 때 중첩 따옴표와 같은 엣지 케이스를 주의해야 하며, 이는 정규 표현식이나 파서 라이브러리를 사용하는 보다 정교한 접근 방식이 필요할 수 있습니다.

## 참조

Kotlin에서 문자열을 다루는 더 많은 컨텍스트를 위해서는 공식 문서를 확인하세요:

- [Kotlin의 문자열 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Kotlin에서 정규 표현식과 파싱을 더 깊이 다루기 위해서:

- [Kotlin 정규 표현식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
