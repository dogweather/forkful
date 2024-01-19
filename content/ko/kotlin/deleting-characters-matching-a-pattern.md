---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
패턴과 일치하는 문자를 삭제하는 것은 문자열에서 특정 규칙 또는 패턴을 따르는 문자를 제거하는 프로그래밍 작업입니다. 프로그래머들은 불필요하거나 의도하지 않은 문자를 제거하여 데이터를 깔끔하게 정리하고, 입력 오류를 줄이기 위해 이 작업을 수행합니다.

## 방법:
Kotlin 코드를 사용하여 문자열에서 패턴에 일치하는 문자를 제거하는 방법을 보여드리겠습니다.
```Kotlin
fun main() {
    val regex = "[0-9]".toRegex()
    val str = "Hello123World456"
    val cleanStr = str.replace(regex, "")
    println(cleanStr)
}
```
위의 코드 결과는 다음과 같습니다.
```
HelloWorld
```
주어진 문자열에서 숫자를 제거하였으니 출력으로 "HelloWorld"만 받게 됩니다.

## 깊게 알아보기:
사실 문자열에서 패턴에 일치하는 문자를 삭제하는 작업은 고대 프로그래밍 언어부터 이용되어왔습니다. 만약 Kotlin을 사용하지 않는다면, JavaScript의 `replace()` 메서드 또는 Python의 `sub()` 메서드를 사용할 수 있습니다. Kotlin에서 이러한 문자 삭제는 `replace()` 함수의 도움으로 가능하게 됩니다. 이 함수는 첫 번째 인수로 패턴을 입력 받아 패턴에 일치하는 모든 문자열을 찾는데, 두 번째 인수로는 대체할 문자열을 입력받습니다.

## 참고:
1. Kotlin 공식 문서: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)
2. 문자열 처리에 대한 자세한 내용: [https://programminghistorian.org/en/lessons/cleaning-ocrd-text-with-regex](https://programminghistorian.org/en/lessons/cleaning-ocrd-text-with-regex)