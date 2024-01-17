---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Arduino: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열에서 특정 패턴과 일치하는 문자를 삭제하는 것은 프로그래머들이 자주하는 작업입니다. 이를 통해 프로그래머들은 코드를 더 간결하고 효율적으로 만들 수 있습니다.

# 방법:

Arduino에서는 다양한 방법으로 문자를 삭제할 수 있습니다. 가장 간단하고 일반적인 방법은 ```String.replace()``` 함수를 사용하는 것입니다. 예를 들어, ```String.replace("a", "")```는 문자열에서 "a"가 포함된 모든 문자를 삭제합니다. 또는 정규표현식을 사용하여 패턴과 일치하는 문자를 삭제할 수도 있습니다. 예를 들어, ```String.replace(/a+/g, "")```는 문자열에서 "a"가 하나 이상 포함된 패턴을 삭제합니다.

## 예제:
```
// "Hello, World!" 문자열에서 ","를 삭제하는 예제
String str = "Hello, World!";
str.replace(",", "");
// str의 결과는 "Hello World!"가 됩니다.
```

## 깊이 있게 알아보기:

이 방법은 다양한 방법으로 구현할 수 있지만, 대부분의 프로그래밍 언어에서는 문자열을 조작하는 함수를 제공합니다. 예를 들어, C 언어에서는 ```strtok()``` 함수를 사용하여 문자열에서 원하는 문자를 삭제할 수 있습니다. 또는 Java에서는 정규표현식을 사용하여 문자를 삭제할 수 있습니다.

# 참고 자료:

- [String.replace() 함수 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/) 
- [C 언어에서 문자열 조작하기](https://www.programiz.com/c-programming/c-strings)
- [Java에서 정규표현식 사용하기](https://www.tutorialspoint.com/java/java_regular_expressions.htm)