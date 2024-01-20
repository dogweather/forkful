---
title:                "정규 표현식을 사용하는 법"
html_title:           "Kotlin: 정규 표현식을 사용하는 법"
simple_title:         "정규 표현식을 사용하는 법"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식을 사용한다는 것은 무엇일까요? 프로그래머들이 이를 하는 이유는 무엇일까요? 정규 표현식은 문자열에서 원하는 텍스트를 찾거나 검색하기 위해 사용되는 패턴이라고 생각할 수 있습니다. 프로그래머들은 이를 사용하여 복잡한 문자열 작업을 더 쉽고 간단하게 수행할 수 있습니다.

## 어떻게:

```Kotlin
val string = "Hello world!"
val pattern = Regex("[a-zA-Z]+")
val result = pattern.find(string)
print(result?.value)
```

위의 코드는 "Hello world!"라는 문자열에서 알파벳으로 이루어진 부분을 찾아 출력하는 간단한 예제입니다. "pattern"은 찾고자 하는 패턴을 나타내는 정규 표현식이며, "result"는 해당 패턴을 찾아낸 결과를 가지고 있습니다.

## 깊이 파고들기:

정규 표현식은 1950년대 미국 수학자인 Stephen Kleene이 고안한 것으로, 컴퓨터 과학에서 많은 사용을 보이고 있습니다. 다른 대안들로는 문자열 메소드나 라이브러리를 사용하는 것이 있지만, 정규 표현식은 더 유연하고 간결한 코드 작성을 가능하게 해주기 때문에 많은 프로그래머들에게 인기가 있습니다. Kotlin에서는 java.util.regex 모듈을 통해 정규 표현식을 지원하며, Regex 클래스를 통해 사용할 수 있습니다.

## 관련 자료:

- [수업 영상: 코틀린과 정규 표현식](https://youtu.be/hPVAo06yN_g)