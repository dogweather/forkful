---
title:                "문자열 연결"
html_title:           "Java: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결이란 무엇일까요? 이것은 단순히 문자열을 결합하는 것을 의미합니다. 일반적으로 프로그래머들은 문자열 연결을 사용하여 긴 문자열을 만들거나 다른 데이터 유형을 문자열로 변환하기 위해 사용합니다.

## 방법:

### 문자열 연결 방법 1: "+" 기호 사용하기
```Java
// 문자열 두 개를 결합하기
String result1 = "안녕하세요" + " 반가워요";
System.out.println(result1);
// 출력 결과: 안녕하세요 반가워요

### 문자열 연결 방법 2: concat() 메서드 사용하기
```Java
// 문자열 두 개를 concat() 메서드로 결합하기
String result2 = "안녕하세요".concat(" 반가워요");
System.out.println(result2);
// 출력 결과: 안녕하세요 반가워요
```

## 깊이 파헤치기:

### 역사적 배경:
문자열 연결은 자바의 초기 버전부터 사용되어 왔습니다. 초기에는 "+" 기호를 사용하여 문자열을 결합하였으나, 성능상의 이유로 concat() 메서드가 추가되었습니다.

### 대안:
이외에도 문자열 결합을 위한 다양한 방법이 존재합니다. 가장 잘 알려진 방법은 StringBuilder나 StringBuffer를 사용하는 것입니다.

### 구현 세부사항:
문자열 연결은 두 개의 문자열을 결합하고 새로운 문자열을 생성하는 과정입니다. 이는 메모리 관리를 위해 매우 중요합니다. 따라서 문자열 연결을 반복적으로 수행할 때에는 StringBuilder나 StringBuffer를 사용하는 것이 성능상 좋습니다.

## 참고 자료:

- [Java Tutorial: Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java String Concatenation](https://www.javatpoint.com/java-string-concatenation)