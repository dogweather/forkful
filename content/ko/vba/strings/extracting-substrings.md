---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:18.909668-07:00
description: "\uBC29\uBC95: VBA\uC5D0\uC11C\uB294 \uC8FC\uB85C `Mid`, `Left`, \uADF8\
  \uB9AC\uACE0 `Right` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\
  \uC744 \uCD94\uCD9C\uD569\uB2C8\uB2E4. \uC544\uB798\uC5D0\uC11C \uC774\uB7EC\uD55C\
  \ \uD568\uC218\uB4E4\uC744 \uC608\uC81C\uC640 \uD568\uAED8 \uC0B4\uD3B4\uBCF4\uACA0\
  \uC2B5\uB2C8\uB2E4: 1. **Mid**: \uC9C0\uC815\uB41C \uC704\uCE58\uC5D0\uC11C \uC2DC\
  \uC791\uD558\uB294 \uBB38\uC790\uC5F4\uC758 \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744\
  \ \uCD94\uCD9C\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.962894-06:00'
model: gpt-4-0125-preview
summary: "VBA\uC5D0\uC11C\uB294 \uC8FC\uB85C `Mid`, `Left`, \uADF8\uB9AC\uACE0 `Right`\
  \ \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\
  \uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
weight: 6
---

## 방법:
VBA에서는 주로 `Mid`, `Left`, 그리고 `Right` 함수를 사용하여 문자열을 추출합니다. 아래에서 이러한 함수들을 예제와 함께 살펴보겠습니다:

1. **Mid**: 지정된 위치에서 시작하는 문자열의 부분 문자열을 추출합니다.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Mid(exampleString, 7, 5)
   Debug.Print result  ' 출력: World
   ```

2. **Left**: 문자열의 왼쪽에서 지정된 숫자만큼의 문자로 구성된 부분 문자열을 추출합니다.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Left(exampleString, 5)
   Debug.Print result  ' 출력: Hello
   ```

3. **Right**: 문자열의 오른쪽에서 지정된 숫자만큼의 문자로 구성된 부분 문자열을 추출합니다.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Right(exampleString, 5)
   Debug.Print result  ' 출력: World
   ```

이 기본적인 함수들은 VBA에서의 부분 문자열 추출의 기반을 형성하며, 문자열 조작에 대한 강력하고 직관적인 접근 방식을 제공합니다.

## 심화 탐구:
역사적으로, 프로그래밍에서 문자열 조작 능력은 필수적이었으며, 개인용 컴퓨터의 초기 시절에 이 능력을 대중화한 것 중 BASIC(VBA의 선구자)이 있었습니다. VBA의 `Mid`, `Left`, 및 `Right` 함수는 이러한 유산을 계승하여, 현대 프로그래머들에게 간소화된 인터페이스를 제공합니다.

이러한 함수들은 많은 작업에 매우 효과적이지만, 새로운 언어에서의 정규 표현식의 등장은 텍스트 작업을 위한 더 강력하고 유연한 방법을 제공했습니다. 그럼에도 불구하고, 전통적인 VBA 부분 문자열 함수의 즉각적인 단순함과 이용 가능성은 빠른 작업과 프로그래밍 초보자에게 완벽하게 적합합니다.

VBA는 더 복잡한 파싱과 문자열 내 검색 작업에 대해 `Like` 연산자와 `VBScript.RegExp` 객체를 통한 패턴 매칭을 지원하지만, 이러한 기능들을 효과적으로 사용하기 위해서는 조금 더 많은 설정과 이해가 필요합니다. 이런 도구들이 더 큰 힘을 제공함에도 불구하고, `Mid`, `Left`, `Right`의 직관적인 특성은 많은 VBA 프로그램에서 그들의 지속적인 관련성과 유용성을 보장합니다.
