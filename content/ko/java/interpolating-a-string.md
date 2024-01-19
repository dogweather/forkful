---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 보간(Interpolating a string)은 문자열 내의 변수를 그 값으로 바꾸는 것입니다. 이렇게 하면 코드를 읽고 이해하는데 더 쉽고 유출되는 정보를 쉽게 다룰 수 있습니다.

## 사용 방법:

자바 15 버전부터는 `formatted()` 메소드를 사용해 문자열 보간을 할 수 있습니다. 아래는 예시입니다:
	
```Java 
int age = 20;
String name = "홍길동";
System.out.println("이름: %s, 나이: %d".formatted(name, age)); 
``` 
	
출력:
	
```Java 
이름: 홍길동, 나이: 20
``` 

보시다시피, `%s`와 `%d`는 `formatted()` 메소드를 통해 `name`과 `age`로 치환됩니다.

## 깊게 알아보기:

1. **역사적 맥락**: 자바는 오래 전부터 문자열 보간을 위한 다양한 방법을 제공했습니다. 이전에는 `String.format()` 메소드를 사용했습니다. 그러나 자바 15 버전에서는 `formatted()` 메소드를 추가하여 작성 능력을 강화했습니다.

2. **대체 방법**: 위에서 언급한 것처럼 이전에는 `String.format()`을 사용했습니다. 또한, `MessageFormat` 또는 `StringBuilder`를 사용하는 등 다른 라이브러리와 클래스를 사용하는 방법도 있습니다.

3. **구현 세부 정보**: `formatted()`와 같은 메소드는 내부적으로 `Formatter` 클래스를 사용해 문자열 보간을 수행합니다. 이는 중괄호 `{}` 안에 있는 인덱스를 참조하여 변환된 값을 대체합니다.

## 참고:

- [Oracle Documentation - formatted()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#formatted(java.lang.Object...))
- [Oracle Documentation - String.format()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))

이정도면 문자열 보간에 대해 충분히 알게 되셨을 것입니다. 이상입니다!