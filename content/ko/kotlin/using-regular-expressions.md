---
title:                "Kotlin: 정규식을 사용하는 방법"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규식을 사용해야 할까요?

정규식은 텍스트 데이터에서 원하는 패턴을 찾아내는 강력한 도구입니다. 이를 이용하면 문자열을 다루는 작업이 훨씬 수월해지며, 코드를 더 간결하고 효율적으로 만들 수 있습니다. 따라서 정규식은 프로그래밍에서 매우 중요한 역할을 합니다.

## 사용 방법

정규식을 사용하기 위해서는 `Regex` 클래스를 이용해야 합니다. 다음은 간단한 예제 코드입니다.

```Kotlin
val regex = Regex("[0-9]+")
val text = "Hello world! 123"
val matches = regex.findAll(text)
for (match in matches) {
    println(match.value)
}
```

위 코드는 문자열 `text`에서 숫자만을 찾아내어 출력하는 예제입니다. `Regex` 클래스의 `findAll` 메서드를 이용하면, 해당 정규식과 일치하는 모든 부분을 `MatchResult` 객체로 반환합니다. 이 객체는 `value` 속성을 통해 일치하는 문자열을 가져올 수 있습니다.

## 깊게 파보기

정규식에는 다양한 메타문자와 문자 클래스, 그리고 수량자 등이 있어 다양한 패턴을 찾을 수 있습니다. 또한, 캡처 그룹을 이용해 원하는 부분을 추출하는 것도 가능합니다. 이런 다양한 기능들을 익히고 활용하면 더욱 정교한 정규식을 만들어낼 수 있습니다.

## 더 알아보기

- [코틀린 공식 문서 - 정규식 사용하기](https://kotlinlang.org/docs/tutorials/kotlin-for-py/regular-expressions.html)
- [정규식 코딩 도장](https://regexr.com/)
- [정규식을 사용한 텍스트 처리 - 박상길 블로그](https://kangsudug.tistory.com/50)

## 참고자료

- [코틀린 공식 문서 - 정규식 사용하기](https://kotlinlang.org/docs/tutorials/kotlin-for-py/regular-expressions.html)
- [정규식 코딩 도장](https://regexr.com/)
- [정규식을 사용한 텍스트 처리 - 박상길 블로그](https://kangsudug.tistory.com/50)