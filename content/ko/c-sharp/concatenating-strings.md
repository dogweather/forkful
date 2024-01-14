---
title:    "C#: 문자열 연결하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것이 중요한 이유는 문자열을 조합하여 더 많은 정보를 표시할 수 있기 때문입니다. 예를 들어, 사용자 이름과 성씨를 합쳐서 전체 이름을 나타내는 등의 경우에 매우 유용합니다.

## 방법
```C#
string firstName = "이름";
string lastName = "성씨";

// 문자열을 연결하는 방법 1: + 연산자 사용
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
// 결과: 이름 성씨

// 문자열을 연결하는 방법 2: string.Format() 메소드 사용
string fullName = string.Format("{0} {1}", firstName, lastName);
Console.WriteLine(fullName);
// 결과: 이름 성씨

// 문자열을 연결하는 방법 3: $ 표시 사용 (C# 6 이상)
string fullName = $"{firstName} {lastName}";
Console.WriteLine(fullName);
// 결과: 이름 성씨
```

## 깊게 파헤치기
문자열을 연결할 때 주의할 점은 메모리 관리입니다. 예를 들어, 문자열을 연결할 때마다 새로운 객체가 생성되므로, 많은 수의 문자열을 연결하는 작업을 수행할 경우 메모리 부족 문제가 발생할 수 있습니다. 따라서 큰 문자열을 처리할 때는 StringBuilder 클래스를 사용하여 메모리 관리를 최적화하는 것이 좋습니다.

## 참고 자료
- [C# 문자열 연결 방법](https://docs.microsoft.com/ko-kr/dotnet/csharp/how-to/concatenate-multiple-strings)
- [C# 문자열 포맷팅](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/strings/formatting-strings)
- [C# StringBuilder 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.stringbuilder)
- [메모리 관리에 대한 고려사항](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/memory-considerations)