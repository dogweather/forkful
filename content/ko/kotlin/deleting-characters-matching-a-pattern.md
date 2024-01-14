---
title:    "Kotlin: 패턴과 일치하는 문자 삭제하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"## 왜"
"특정 패턴과 일치하는 문자를 삭제하는 것"의 이유에 대해 1-2 문장으로 설명합니다.

"## 어떻게"
"```Kotlin
fun main() {
    val str = "Kotlin is fun!"
    val newStr = str.replace(Regex("[a-zA-Z]+"), "") // 패턴에 해당하는 문자를 빈 문자열로 대체합니다.
    println(newStr) // 출력 결과: "!"
}
```"
위와 같은 코틀린 코드 블록을 사용하여 예시와 출력 결과를 보여줍니다.

"## 딥 다이브"
여러분이 사용하는 텍스트 에디터, 웹 애플리케이션 등에서 문자열을 처리할 때, 패턴 일치 검색을 통해 문자를 삭제할 수 있는 다양한 방법이 있습니다. 정규식을 사용하는 방법 외에도, 문자열을 띄어쓰기 단위로 나누어 배열로 변환한 뒤 필요한 문자열을 제거하는 방법 등도 있습니다. 이러한 방법들을 자세히 알아보고, 어떤 상황에서 어떤 방법이 더 적합한지 알아봅니다.

"## 또 다른 방법들"
- [Kotlin 공식 문서 - 문자열 다루기](https://kotlinlang.org/docs/reference/strings.html)
- [Kotlin vs Java 문자열 관련 기능 비교](https://medium.com/circleci/supporting-string-manipulation-using-kotlin-8c55da20b272)
- [정규식 테스트 및 예제](https://regex101.com/)