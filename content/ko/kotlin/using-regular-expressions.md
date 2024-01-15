---
title:                "정규식 사용하기"
html_title:           "Kotlin: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 간단합니다. 그것은 특정한 패턴을 찾고, 변경하고, 추출하는 데에 매우 유용하기 때문입니다. 이것은 텍스트 처리 작업을 더 효율적으로 만들어줍니다.

## 어떻게
정규 표현식을 사용하기 위해서는 ```Regex``` 클래스를 사용해야 합니다. 이 클래스는 문자열을 표현식과 비교하고 일치하는 패턴을 찾아내는 기능을 제공합니다. 예를 들어, 특정 이메일 양식을 검증하기 위해 다음과 같이 코드를 작성할 수 있습니다.

```Kotlin 
val emailRegex = Regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$")
val email = "example@example.com"
if (email.matches(emailRegex)) {
    println("Valid email address")
} else {
    println("Invalid email address")
}
```

위 예제에서 우리는 이메일 주소를 검증하기 위해 정규 표현식을 사용했습니다. 우리는 이메일 양식의 패턴에 맞게 작성된 문자열이 일치하는지 확인하고, 이에 따라 유효한 이메일 주소인지 아닌지를 출력합니다.

## 딥 다이브
정규 표현식을 다루기 위해서는 정규 표현식 구문에 대한 이해가 필요합니다. 예를 들어, ```^```는 문자열의 시작을 나타내는 메타 문자입니다. 또한 ```+```는 이전 패턴의 하나 이상의 반복을 나타내는 메타 문자입니다. 이와 같은 정규 표현식의 구문을 이해하면 더 복잡한 패턴을 구성할 수 있습니다.

## 더 알아보기
- [Kotlin Regular Expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regular Expressions Info](https://www.regular-expressions.info/)
- [Mastering Regular Expressions](http://www.masteringregex.com/)