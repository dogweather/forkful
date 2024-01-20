---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열의 길이를 찾는 것은 특정 문자열이 얼마나 많은 문자로 이루어져 있는지 확인하는 것을 의미합니다. 이는 데이터 유효성 검사, 문자열 처리, 또는 문서 생성에서 중요한 기능을 수행하기 때문에 프로그래머들이 자주 사용합니다.

## 바로 실행하기:

문자열의 길이를 찾기 위해 C#에서는 'Length' 프로퍼티를 사용합니다. 아래 코드를 확인해보세요.

```C#
string sampleString = "안녕하세요!";
int lengthOfString = sampleString.Length;

Console.WriteLine(lengthOfString); //output: 6
```
이 예제에서, '안녕하세요!'라는 문자열의 길이는 6임을 알 수 있습니다.

## 깊이 파고들기:

**역사적 맥락:** 문자열 길이의 개념은 프로그래밍 언어의 초기부터 존재했습니다. 문장이나 메시지를 구성하는 글자의 수를 셀 때 사용되었습니다. C#도 이 트레디션을 계승하였습니다.

**대체 방법:** C#에서는 'Length' 프로퍼티 외에도 Linq를 사용하여 문자열의 길이를 찾을 수 있습니다. 하지만 'Length' 프로퍼티가 효율적이므로 굳이 이 방법을 사용할 필요는 없습니다.

```C#
int length = "안녕하세요!".Count();
```
**실행 세부 사항:** C#에서 문자열의 'Length' 프로퍼티는 O(1) 시간복잡도를 가집니다. 이는 문자열의 길이를 알아내는데 있어서 효율적이며, 복잡한 계산이나 추가 메모리를 필요로하지 않습니다.

## 참고자료:

1. [MSDN : String.Length 속성](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.length?view=net-5.0): Microsoft의 공식 문서에서는 'Length' 프로퍼티에 대하여 더욱 자세히 설명하고 있습니다.
2. [stackoverflow : C# 문자열 길이 구하기](https://stackoverflow.com/questions/30863157/how-to-get-the-length-of-a-string-in-c): 이 Stack Overflow 질문에서는 문자열의 길이를 구하는 여러 가지 방법에 대해 토론하고 있습니다.