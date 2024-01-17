---
title:                "문자열 이어 붙이기"
html_title:           "C: 문자열 이어 붙이기"
simple_title:         "문자열 이어 붙이기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무슨 내용이고 왜 하는가?

컴퓨터 프로그래밍에서 문자열을 연결(concatenate)한다는 것은 무엇일까요? 문자열을 연결한다는 것은 두 개 이상의 문자열을 합쳐서 하나의 긴 문자열로 만드는 것을 의미합니다. 프로그래머들은 일반적으로 이것을 문자열을 조작하고 조합하기 위해 사용합니다.

## 하는 방법:

문자열을 연결하는 방법은 다양합니다. 가장 기본적인 방법은 문자열 합치기 함수인 strcat()을 사용하는 것입니다. strcat() 함수는 첫 번째 문자열의 끝에 두 번째 문자열의 내용을 추가해주는 역할을 합니다. 예를 들어, 다음과 같은 코드를 작성하면:

```C
char firstString[10] = "Hello";
char secondString[10] = "World";
strcat(firstString, secondString);
printf("%s", firstString);
```
출력 결과는 "HelloWorld"가 됩니다. 따라서 두 번째 문자열이 첫 번째 문자열에 추가되어 하나의 문자열이 되었습니다.

## 조금 더 깊게 들어가보겠습니다:

문자열을 연결하는 것은 어떻게 프로그래밍 언어의 중요한 기능이 되었을까요? 이것은 문자열을 조작하고 처리하는 데 있어서 매우 유용합니다. 또한 여러 문자열을 조합해야 할 때, 하나의 큰 문자열로 합쳐서 처리하는 것이 더 효율적이기 때문에 이 기능이 필수적입니다. 그리고 많은 다른 언어들에서도 문자열을 연결할 수 있는 기능을 제공합니다. 하지만 이러한 기능들은 동적인 프로그래밍 언어에서는 보다 유용하게 사용됩니다.

## 관련 자료:

문자열을 연결하는 기능은 많은 프로그래밍 언어에서 제공되지만, 각 언어마다 약간씩 다른 구현 방식이 있을 수 있습니다. 이를 더 알기 위해서는 많은 예제를 찾아보고 다른 언어들의 구현 방식을 비교해봐야 합니다. 관련 자료들은 다음과 같습니다:

- [C 언어로 문자열을 연결하는 방법](https://www.programiz.com/c-programming/library-function/string-h/strcat)
- [Java 언어로 문자열을 연결하는 방법](https://www.tutorialspoint.com/java/lang/string_concat.htm)
- [Python 언어로 문자열을 연결하는 방법](https://www.geeksforgeeks.org/python-program-to-concatenate-two-strings/)