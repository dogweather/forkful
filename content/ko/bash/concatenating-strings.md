---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 그리고 왜?

문자열 연결(string concatenation)은 두 개 이상의 문자열을 하나로 결합하는 과정입니다. 프로그래머는 변수, 출력, 및 통신메시지를 생성할 때 자주 활용합니다.

## 어떻게 하나:

다음 예제들은 문자열 연결에 널리 사용되는 Bash 명령입니다:

### 방법1: += 를 사용하여 문자열 연결
```Bash
string1="안녕,"
string1+=" 여러분."
echo $string1
```
출력:
```Bash
안녕, 여러분.
```

### 방법2: {}를 이용해 문자열 연결 
```Bash
string1="안녕"
string2=" 여러분."
echo ${string1}${string2}
```
출력:
```Bash
안녕 여러분.
```

## 깊게 알아보기

### 역사적 배경

문자열 연결의 개념은 프로그래밍의 초기 시절부터 있었고 Bash는 Unix Shell의 한 종류로, 아주 간단하면서도 강력한 문자열 조작 기능을 제공합니다.

### 대안들

Bash 외에도 여러 언어에서 문자열 합치기를 지원합니다. Python, Java, 자바스크립트 등에서도 유사한 연산이 가능합니다.

### 실행 세부정보

Bash에서 문자열 연결의 실행역시 매우 효과적이다.문자열 연결 연산의 대부분은 시간 복잡도가 O(1)입니다. 즉, 연결할 문자열의 길이에 관계없이 거의 일정한 시간이 소요됩니다.

## 참고자료

- Bash 문자열 연결에 대한 더 깊이있는 이해를 위한 링크: [문자열 연결 - Bash](https://linuxize.com/post/bash-concatenate-strings/)
  
- 다른 언어에서의 문자열 연결 방법를 알아보는 링크 :
    - [Python - 문자열 연결](https://www.w3schools.com/python/gloss_python_string_concatenation.asp)
    - [Java - 문자열 연결](https://www.w3schools.com/java/java_strings.asp)
    - [JavaScript - 문자열 연결](https://www.w3schools.com/js/js_string_methods.asp)