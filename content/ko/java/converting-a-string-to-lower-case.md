---
title:                "문자열을 소문자로 변환하기"
html_title:           "Java: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 소문자로 변환하는 것은 문자열에서 대소문자 구분이 없는 검색, 비교 또는 일치 여부를 확인하기 위해 프로그래머들이 수행하는 작업입니다.

## 방법:
```Java
public class Example {
   public static void main(String[] args) {
      String str = "HeLLo wOrld!";
      System.out.println(str.toLowerCase());
   }
}
```
```Java
Output: hello world!
```

## 깊이 파고들기:
- 과거에는 문자열에 대소문자 변환을 수작업으로 해야했지만, 자바 1.0 이후에는 String 클래스의 toLowerCase() 메소드를 사용하여 간편하게 변환할 수 있게 되었습니다.
- 소문자 변환 외에도 equalsIgnoreCase() 메소드를 사용하여 대소문자 구분 없이 문자열을 비교할 수도 있습니다.
- 문자열 변환 작업은 문자열 길이에 따라 성능이 달라질 수 있으므로, 대부분의 경우에는 프로그래머의 개인적인 판단에 따라 직접 변환할지 메소드를 사용할지 결정하면 됩니다.

## 관련 정보:
- 자바 String 클래스: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- equalsIgnoreCase() 메소드: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#equalsIgnoreCase(java.lang.String)