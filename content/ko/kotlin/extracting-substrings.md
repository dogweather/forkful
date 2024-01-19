---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 특정 부분을 추출하는 것이 '서브스트링 추출'입니다. 프로그래머들은 데이터를 정제하고, 구분하고, 분석하는 과정에서 이 기능을 사용하곤 합니다.

## 어떻게 하는가:

서브스트링 추출은 코틀린에서 매우 직관적입니다.

```Kotlin
val string = "안녕하세요, 저는 코틀린 입니다."
val substring = string.substring(0, 5)
println(substring)
```

출력 결과는 "안녕하세요"입니다.

## 깊은 탐색:

서브스트링을 추출하는 능력은 문자열 처리의 핵심 부분입니다. 코틀린은 자바에서 오랜 역사를 가지고 있는 문자열 처리 기능을 그대로 물려받았습니다.

알TERNATIVES 이름에서 서브스트링을 추출하는 다른 방법으로는 `split` 함수를 사용하는 것이 있습니다. 하지만, 이 방법은 구분자가 필요하고 조금 더 복잡한 로직이 필요하다는 단점이 있습니다.

코틀린은 `substring` 함수를 사용하여 필요한 부분만큼만 추출합니다. `substring`은 시작 인덱스와 종료 인덱스를 포함해서 이 범위에 있는 모든 문자를 반환합니다.

## 참고 자료:

더 다양한 기능과 정보를 얻고 싶다면 아래 링크를 참조해 보세요.
- 공식 코틀린 문서 : https://kotlinlang.org/docs/reference/
- 코틀린 문자열 처리 튜토리얼: https://developer.android.com/kotlin/learn#manipulating-text
- 코틀린으로 서브스트링 추출하기: https://stackoverflow.com/questions/36574183/how-to-get-substring-in-kotlin