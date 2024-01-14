---
title:                "C#: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜: 

텍스트를 찾고 바꾸는 작업을 수행하는 이유는 무엇일까요? 많은 프로그래머들은 소프트웨어를 개발하다보면 다양한 이유로 텍스트를 수정해야 할 때가 있습니다. 예를 들어, 코드에서 일부 단어를 바꾸거나 사용자 입력값을 정제할 때 등 많은 경우에 텍스트를 검색하고 바꾸어야 할 수 있습니다.

## 어떻게:

```C#
// 문자열 선언
string str = "안녕하세요, 친구들! 오늘은 코딩을 배워보겠습니다.";
Console.WriteLine("원본 문자열: " + str);

// "안녕하세요"를 "Hello"로 바꾸기
str = str.Replace("안녕하세요", "Hello");
Console.WriteLine("변경된 문자열: " + str);

// "배워보겠습니다"를 "learning"으로 바꾸기
str = str.Replace("배워보겠습니다", "learning");
Console.WriteLine("최종 문자열: " + str);
```

위의 예시 코드를 보면, `Replace()` 메소드를 이용하여 텍스트를 쉽게 찾고 바꿀 수 있습니다. 첫 번째 인자에는 바꿀 문자열을, 두 번째 인자에는 새로운 문자열을 넣어주면 됩니다. 이 외에도 `Regex` 클래스를 이용하여 더욱 복잡한 패턴의 텍스트를 검색하고 바꿀 수 있습니다.

## 딥 다이브:

텍스트를 찾고 바꾸는 작업은 간단해 보일 수 있지만, 프로그래머들은 이 작업에서 여러 가지 유용한 팁을 알아두면 좋습니다. 예를 들어, `Replace()` 메소드는 대소문자를 구분하므로 반드시 확인하고 사용해야 합니다. 또한, `Regex` 클래스를 이용할 때 정규식 패턴을 제대로 작성하는 것이 중요합니다.

## 참고 프로그램:

- [C# Replace() 메소드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.replace?view=netframework-4.8)
- [C# Regex 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8)
- [C# 문자열 관련 문서](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/strings/)