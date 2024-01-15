---
title:                "문자열 대문자로 변환하기"
html_title:           "Java: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 대문자로 변환하는 것을 왜 시도해야 할까요? 그 이유는 무엇인가요? 문자열을 대문자로 바꾸는 것은 컴퓨터 프로그래밍에서 매우 일반적이고 유용한 작업입니다. 예를 들어, 사용자의 입력을 검증하거나 출력을 포매팅하는 데에 사용할 수 있습니다.

## 어떻게?

자바에서 문자열을 대문자로 바꾸는 방법은 간단합니다. 아래의 코드 블록을 참고해주세요.

```Java
String str = "hello world";
String uppercaseStr = str.toUpperCase();
System.out.println(uppercaseStr);
```

위 코드는 "HELLO WORLD"를 출력합니다. 대문자로 변환하는 것 외에도, 자바에는 다양한 문자열 변환 메소드가 있으며, 이를 사용하여 비슷한 작업을 수행할 수 있습니다. 예를 들어, `toLowerCase()` 메소드는 문자열을 소문자로 변환하고, `replace()` 메소드는 문자열 안의 특정 문자를 다른 문자로 대체할 수 있습니다.

## 더 들어가기

문자열을 대문자로 바꾸는 데에 앞서 변환된 문자열을 저장할 변수를 선언해야합니다. 그렇지 않으면 원래의 문자열이 수정되는 위험이 있습니다. 예를 들어, 아래의 코드를 살펴봅시다.

```Java
String str = "hello world";
str.toUpperCase();
System.out.println(str);
```

위 코드는 "hello world"를 출력합니다. 왜냐하면 `toUpperCase()` 메소드는 문자열을 수정하는 것이 아니라, 대문자로 변환된 새로운 문자열을 반환하는 것이기 때문입니다. 따라서, 문자열을 손상시키지 않으려면 변환된 문자열을 새로운 변수에 저장하는 것이 중요합니다.

## 더보기

- [Java String 관련 메소드들](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [자바 앱 개발을 위한 기본 용어 정리](https://javapapers.com/core-java/java-terminology/)
- [자바 프로그래밍 초보자를 위한 튜토리얼](https://beginnersbook.com/java-tutorial-for-beginners/)