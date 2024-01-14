---
title:                "C#: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
왜 누군가가 문자열을 연결하는 것에 집중할까요? 문자열을 연결하는 것은 프로그래밍에서 매우 중요한 역할을 합니다. 이를 통해 여러 개의 문자열을 하나의 문자열로 결합하여 보다 복잡한 데이터를 다룰 수 있습니다.

## 방법
아래는 C#언어로 문자열을 연결하는 방법을 보여주는 예시 코드입니다.

```C#
string firstName = "김";
string lastName = "철수";
string fullName = firstName + lastName;

Console.WriteLine(fullName);
```

위 코드를 실행하면 "김철수"라는 문자열이 출력됩니다. 이처럼 "+" 기호를 사용하여 두 문자열을 연결할 수 있습니다.

## 깊게 파보기
단순히 문자열을 연결하는 것 외에도, C#에는 여러가지 문자열 연결 관련 기능이 있습니다. 예를 들어, 문자열 사이에 다른 문자열을 삽입하는 `String.Format()` 메소드나 동일한 문자열을 반복하여 연결하는 `String.Concat()` 메소드 등이 있습니다. 이러한 기능들을 적절히 활용하면 보다 다양한 문자열 조작이 가능해집니다.

## 또 다른 자료
- [C# String 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.string?view=netcore-3.1)
- [C# 문자열 연결 관련 메소드 모음](https://www.tutorialsteacher.com/csharp/csharp-string-class)
- [C# 문자열 연결 예시 코드](https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm)

---

## 참고 자료
- [Markdown 기본 문법](https://gist.github.com/ihoneymon/652be052a0727ad59601)
- [C# 공식 문서 번역 프로젝트](https://github.com/dotnet/csharpdocs)