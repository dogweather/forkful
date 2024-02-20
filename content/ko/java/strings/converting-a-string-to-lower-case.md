---
date: 2024-01-20 17:38:37.032628-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294\
  \ \uAC83\uC740 \uB300\uBB38\uC790\uB85C \uB41C \uD14D\uC2A4\uD2B8\uB97C \uC804\uBD80\
  \ \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC774 \uC791\uC5C5\uC744 \uD558\uAE30\uB85C\
  \ \uD558\uB294 \uC774\uC720\uB294 \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBCC4\uD558\
  \uC9C0 \uC54A\uB294 \uAC80\uC0C9, \uC815\uB82C \uB610\uB294 \uB370\uC774\uD130 \uC815\
  \uADDC\uD654\uB97C \uC704\uD574\uC11C\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.936068
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\
  \uC740 \uB300\uBB38\uC790\uB85C \uB41C \uD14D\uC2A4\uD2B8\uB97C \uC804\uBD80 \uC18C\
  \uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uAC00 \uC774 \uC791\uC5C5\uC744 \uD558\uAE30\uB85C \uD558\
  \uB294 \uC774\uC720\uB294 \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBCC4\uD558\uC9C0\
  \ \uC54A\uB294 \uAC80\uC0C9, \uC815\uB82C \uB610\uB294 \uB370\uC774\uD130 \uC815\
  \uADDC\uD654\uB97C \uC704\uD574\uC11C\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 바꾸는 것은 대문자로 된 텍스트를 전부 소문자로 변환하는 과정입니다. 프로그래머가 이 작업을 하기로 하는 이유는 대소문자를 구별하지 않는 검색, 정렬 또는 데이터 정규화를 위해서입니다.

## How to: (방법)
Java에서 문자열을 소문자로 변환하는 가장 기본적인 방법은 `toLowerCase()` 메서드를 사용하는 것입니다. 아래에 간단한 예제를 보여드립니다.

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String text = "Hello, World!";
        String lowerCaseText = text.toLowerCase();
        System.out.println(lowerCaseText);
    }
}
```

출력:
```
hello, world!
```

## Deep Dive (심층 탐구)
### 역사적 배경
소문자로 변환하는 기능은 자바 초기 버전부터 제공되었습니다. 이는 텍스트 처리가 필수적인 부분이기 때문에 기본 문자열 클래스의 핵심 기능 중 하나로 자리 잡았습니다.

### 대안
다국어 지원이 필요한 경우, `Locale` 클래스와 함께 `toLowerCase()`를 사용할 수 있습니다. 이렇게 하면 특정 언어에 맞춘 소문자 변환 규칙을 적용할 수 있습니다.

```java
String koreanText = "안녕하세요!";
String lowerCaseKoreanText = koreanText.toLowerCase(Locale.KOREAN);
System.out.println(lowerCaseKoreanText);
```

### 구현 세부 사항
`toLowerCase()` 메서드는 UTF-16을 사용하는 String 클래스에서 제공합니다. 이 메서드는 내부적으로 문자마다 정의된 소문자 변환 규칙에 따라 각 문자를 검사하고 변환합니다.

## See Also (참고할 만한 자료)
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Locale Class in Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)

이 문서에서는 Java 문자열을 소문자로 변환하는 방법과 그 사용례를 간략하게 소개했습니다. 더 자세한 정보를 원하신다면 위의 참고 자료 링크를 확인해보세요.
