---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 간단합니다. 당신의 쉘 스크립트를 더 유연하고 간단하게 만들기 위해서입니다.

## 사용 방법

### 기본 문법

정규 표현식은 특정 패턴과 일치하는 문자열을 찾기 위해 사용됩니다. 가장 기본적인 문법은 다음과 같습니다:

```Bash
pattern="test"
[[ "This is a test" =~ $pattern ]]
echo $?
```
출력: `0` (참)

`[[ ... =~ ... ]]` 문법을 사용하여 `=~` 뒤에 위치한 패턴과 문자열을 비교합니다. 이 비교 결과는 `true(0)` 또는 `false(1)`로 출력됩니다. 이 경우 `"This is a test"` 문자열은 `"test"` 패턴과 일치하므로 결과는 참(`0`)입니다.

### 와일드카드

정규 표현식에서는 `*` 및 `?`와 같은 와일드카드를 사용할 수 있습니다. 이들은 문자열에서 특정 패턴과 일치하는 부분을 나타냅니다. 예를 들어:

```Bash
pattern="He*l?"
[[ "Hello" =~ $pattern ]]
echo $?
```
출력: `0` (참)

위의 예제에서 `"Hello"` 문자열은 `"He*l?"` 패턴과 일치하므로 결과는 참(`0`)입니다. `*`는 0개 이상의 문자를 나타내고 `?`는 한 개의 문자를 나타냅니다.

### 그룹화

정규 표현식에서는 이전에 매칭한 문자열을 다시 사용할 수 있도록 그룹을 지정할 수 있습니다. 이를 위해서는 `()`를 사용합니다. 예를 들어:

```Bash
pattern="Hello (World)?"
[[ "Hello" =~ $pattern ]]
echo $?
[[ "Hello World" =~ $pattern ]]
echo $?
```
출력: `1` (거짓)
출력: `0` (참)

첫 번째 예제에서는 `"Hello"` 문자열은 `"Hello (World)?"` 패턴과 일치하지 않으므로 결과는 거짓(`1`)입니다. 두 번째 예제에서는 `"Hello World"` 문자열은 `"Hello (World)?"` 패턴과 일치하므로 결과는 참(`0`)입니다.

## 깊이 있는 이야기

추가적인 공부를 원한다면 다음 자료들을 참고하세요:

- [Bash에서 정규 표현식 사용 방법](https://linuxconfig.org/bash-regular-expressions)
- [정규 표현식으로 문자열 분리하기](https://www.lifewire.com/use-regular-expression-examples-2202168)
- [Linux에서의 정규 표현식 101](https://www.geeksforgeeks.org/regular-expressions-linux-normal-examples/)