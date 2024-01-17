---
title:                "부분 문자열 추출"
html_title:           "C#: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇& 왜?

서브스트링 추출이 무엇인지 설명하고 프로그래머들이 이 작업을 왜 하는지 알아봅시다.

서브스트링 추출은 문자열에서 더 작은 문자열을 추출하는 작업입니다. 이 작업을 수행하는 이유는 다양할 수 있지만 가장 일반적인 이유는 특정 문자열을 찾거나 원하는 형식으로 문자열을 변환하기 위해서입니다.

## 방법:

아래의 ```C#... ``` 코드 블록 안에서 코딩 예제와 샘플 출력을 확인해 봅시다.

```C#
// 문자열에서 서브스트링 추출하기
string myString = "안녕하세요, 프로그래머입니다.";
string substring = myString.Substring(3, 5); // "녕하세"를 추출

Console.WriteLine(substring); // 출력: 녕하세
```

위의 예제에서는 "안녕하세요" 문자열에서 인덱스 3부터 시작하여 길이 5인 문자열을 추출합니다.

## 깊이 파고들기:

1. 역사적 배경: 서브스트링 추출은 이전부터 존재하는 기술입니다. 예전에는 다양한 언어에서 이 작업을 수행하기 위해 많은 코드를 작성해야 했지만 C#은 간편한 메서드를 제공하여 이 작업을 쉽게 할 수 있게 해주었습니다.

2. 대체 방법: C# 외에도 다른 언어에서도 서브스트링 추출을 수행할 수 있지만 이에는 다양한 방법이 존재합니다. 각 언어마다 메서드 이름이나 파라미터들이 조금씩 다를 수 있으므로 문서를 참고해야 합니다.

3. 구현 세부사항: C#의 Substring 메서드는 문자열에서 인덱스와 길이를 지정하여 서브스트링을 추출할 수 있게 해줍니다. 이는 메서드의 두 번째 파라미터에 음수값을 넣는 것으로도 구현 가능합니다. 또한 이 메서드는 문자열을 수정하는 것이 아니라 새로운 문자열을 생성하여 반환하기 때문에 기존의 문자열은 그대로 유지됩니다.

## 참조할 자료:

- [Microsoft Docs: Substring 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.substring)
- [Microsoft Docs: C#으로 시작하는 프로그래밍](https://docs.microsoft.com/ko-kr/dotnet/csharp/tour-of-csharp/)