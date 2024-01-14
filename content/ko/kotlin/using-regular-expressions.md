---
title:    "Kotlin: 정규 표현식 사용하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 간단합니다. 정규 표현식은 특정한 패턴에 대한 문자열 매칭을 간단하게 수행할 수 있는 강력한 도구이기 때문입니다. 이를 통해 복잡한 문자열 작업을 손쉽게 처리할 수 있으며, 효율적인 문자열 조작에 도움이 됩니다.

## 사용 방법

정규 표현식을 사용하기 위해서는 먼저 Regex 클래스의 인스턴스를 생성해야 합니다. 이 클래스는 컴파일된 정규 표현식을 저장하고, 패턴 매칭에 사용됩니다. 아래는 정규 표현식을 이용해 특정 문자열에서 숫자만 추출하는 예시 코드입니다.

```Kotlin
val regex = Regex("[0-9]+") // 정규 표현식 생성
val str = "Hello 123 Kotlin!!"
val matchResult = regex.find(str) // 문자열에서 숫자 매칭
println(matchResult?.value) // "123" 출력
```

위 코드에서는 "Hello 123 Kotlin!!" 문자열에서 "123"이라는 숫자 패턴을 찾아 출력합니다. Regex 클래스의 find() 메소드는 첫 번째로 매칭되는 패턴을 찾아주기 때문에, 한 번에 한 개의 결과만으로 이 작업을 처리할 수 있습니다.

## 더 깊게 들어가기

정규 표현식은 많은 다양한 기능을 제공하며, 특정 패턴을 찾는 것 외에도 문자열을 분리하거나 치환하는 등 다양한 용도로 사용할 수 있습니다. 정규 표현식을 자세히 공부하고 더욱 다양한 용도로 활용하기 위해서는 문서나 온라인 자료를 참고하는 것이 좋습니다. 

## 더 읽어보기

- [코틀린 공식 문서: 정규 표현식](https://kotlinlang.org/docs/reference/regular-expression.html)
- [정규 표현식 테스트 사이트](https://regex101.com/)
- [블로그: 코틀린에서 정규 표현식 활용하기](https://blog.jetbrains.com/ko/kotlin/2019/01/using-regular-expressions-in-kotlin/)
- [정규 표현식 튜토리얼](https://www.tutorialspoint.com/regex/)