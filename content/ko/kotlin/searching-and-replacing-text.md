---
title:                "Kotlin: 텍스트 검색 및 바꾸기"
simple_title:         "텍스트 검색 및 바꾸기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 교체하는 것은 프로그래밍 작업에서 매우 일반적이고 중요한 작업입니다. 이를테면, 코드에서 특정한 변수 이름을 일괄적으로 바꾸고 싶을 때가 있을 수 있습니다. 이러한 작업을 수동으로 하면 매우 번거로울 뿐만 아니라 실수하기 쉽습니다. 텍스트를 검색하고 교체할 수 있는 기능을 활용하면 이러한 문제를 더 쉽고 정확하게 해결할 수 있습니다.


## 방법

Kotlin에서 텍스트를 검색하고 교체하는 방법은 매우 간단합니다. 먼저, 텍스트가 포함된 문자열을 선언합니다.

```Kotlin
val text = "Hello, world!"
```

텍스트를 교체하기 전과 교체된 후의 값을 비교하면서 코드를 작성해보겠습니다. 텍스트의 "Hello"를 "Bonjour"로 바꾸는 방법을 살펴보겠습니다.

```Kotlin
println(text)
text.replace("Hello", "Bonjour")
println(text)
```

위 코드의 실행 결과는 다음과 같습니다.

```
Hello, world!
Hello, world!
```

화면에는 두 번째 줄만 출력되지만, 실제로는 텍스트가 "Hello"에서 "Bonjour"로 바뀐 것을 확인할 수 있습니다. 이를 더 명확하게 보기 위해 변수에 교체된 값을 다시 저장하고 출력해보겠습니다.

```Kotlin
val newText = text.replace("Hello", "Bonjour")
println(newText)
```

이번에는 정상적으로 "Bonjour, world!"가 출력되는 것을 확인할 수 있습니다.


## 더 깊게

위 예제에서는 텍스트를 전체적으로 바꾸는 방법을 살펴보았습니다. 하지만 실제 개발하면서는 특정한 패턴을 가진 텍스트만 바꾸고 싶은 경우가 많습니다. 이럴 때는 정규표현식을 사용하여 텍스트를 검색하고 교체할 수 있습니다. Kotlin에서는 ```replace``` 함수의 첫 번째 인자로 정규표현식을 전달할 수 있습니다.

예를 들어, "수지"라는 단어가 포함된 모든 텍스트를 "지수"로 바꾸기 위해서는 다음과 같이 정규표현식을 사용할 수 있습니다.

```Kotlin
val newText = text.replace("수지".toRegex(), "지수")
```

또한, 여러 개의 문자열을 한 번에 바꾸기 위해서는 ```replace``` 함수의 두 번째 인자로 ```toRegex()``` 함수로 정규표현식을 전달하는 것도 가능합니다.

마지막으로, 텍스트를 바꿀 때 대소문자를 무시하고 싶은 경우에는 정규표현식에 ```RegexOption.IGNORE_CASE``` 옵션을 추가하여 처리할 수 있습니다.

텍스트를 검색하고 교체하는 기능은 프로그래밍에서 매우 유용하게 사용되는 기능입니다. 이를 활용하여 코드를 더 효율적으로 작성할 수 있도록 노력해보세요.


## 참고

- [Kotlin 공식 문서 - String 처리](https://kotlinlang.org/docs/reference/strings.html)
- [Regex를 이용한 문자열 교체 방법](https://m.blog.naver.com/gngh0101/221447867941)
- [Kotlin Regex 개념 및 활용 방법](https://m.blog.naver.com/Runeglxy/220994826790)