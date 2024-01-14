---
title:                "C#: 문자열의 길이 찾기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

스트링 길이를 찾는 것에 관심을 갖는 이유는 우리가 자주 사용하는 문자열 데이터를 다룰 때 필요하기 때문입니다.

## 어떻게

스트링 길이를 찾는 방법은 C#에서 매우 간단합니다. 우선, `string` 자료형을 사용하는 변수에 대해 `Length` 속성을 호출하면 그 변수에 저장된 문자열의 길이를 알 수 있습니다. 간단한 예시는 다음과 같습니다.

```C#
string name = "홍길동";
int length = name.Length;
Console.WriteLine(length);
```

이 코드를 실행하면 콘솔에는 문자열 "홍길동"의 길이인 3이 출력될 것입니다.

## 딥 다이브

스트링 길이를 찾는 과정에서 우리는 `Length` 속성을 사용하고 있지만, 실제로는 이 속성이 `string` 클래스에서 어떻게 정의되어 있는지 알아야 합니다. `string` 클래스의 소스 코드를 살펴보면 `Length` 속성이 `get` 접근자만 정의되어 있고, 해당 속성에서는 문자열의 길이를 계산하는 로직이 포함되어 있습니다. 이렇게 `get` 접근자를 통해 접근할 수 있는 속성을 우리는 읽기 전용 속성(readonly property)이라고 합니다.

## Git 온라인 리파지토리

* [C# 문자열 길이 찾기 - Microsoft 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.length)
* [예제로 배우는 C# 프로그래밍 - 코뇽이 블로그](https://xonics.github.io/csharp-programming-study-2/)
* [C# String 클래스의 내부 구조 분석 - 소스 코드 네비게이터](https://source.dot.net/#System.Private.CoreLib/String.cs,cd31eecc7dfb5357)
* [C# 문자열 길이 찾기 - 나무위키](https://namu.wiki/w/C)

## 참고

* [C# 속성 사용하기 - Programmers 개발 블로그](https://programmers.co.kr/learn/courses/8/lessons/286)
* [get 접근자와 set 접근자 - MSDN Library](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/classes-and-structs/properties#get-and-set-accessors)