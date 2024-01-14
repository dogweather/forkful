---
title:    "C#: 문자열을 소문자로 변환하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 왜 문자열을 소문자로 변환해야 하는가?
문자열을 소문자로 변환하는 것은 일상적인 작업입니다. 문자열을 사용하여 비교하거나 데이터를 처리할 때, 일관성을 유지하기 위해 소문자로 통일하는 경우가 종종 있기 때문입니다.

## 어떻게 소문자로 변환하는가?
```C#
// 소문자로 변환하기
string input = "Hello World!";
string output = input.ToLower();
Console.WriteLine(output);
```
출력: hello world!

```C#
// 대문자로 변환하기
string input = "Hello World!";
string output = input.ToUpper();
Console.WriteLine(output);
```
출력: HELLO WORLD!

## 깊이 파헤쳐보기
C#에서 문자열을 소문자로 변환하기 위해 ToLower() 메소드를 사용하는 것 외에도, LINQ를 사용하여 모든 문자를 소문자로 변환할 수 있습니다. 또한 특정 CultureInfo를 지정하여 해당 문화권의 소문자 변환 규칙을 따르는 것도 가능합니다.

예를 들어, CultureInfo.InvariantCulture를 지정하면 컴퓨터 기본 설정에 관계없이 일관된 방식으로 소문자로 변환할 수 있습니다.

## 또 다른 관련 정보
### CultureInfo 클래스
https://docs.microsoft.com/ko-kr/dotnet/api/system.globalization.cultureinfo?view=netframework-4.8

### LINQ를 사용한 문자열 소문자 변환 예제
https://www.tutorialspoint.com/linq/linq_lowercase_array_char.htm

### 문자열 비교를 위한 대소문자 변환
https://docs.microsoft.com/ko-kr/dotnet/api/system.stringcomparison?view=netframework-4.8

## 참고 자료
```See Also
https://docs.microsoft.com/ko-kr/dotnet/api/system.string?view=netframework-4.8
https://www.c-sharpcorner.com/blogs/to-lower-and-to-upper-case-in-c-sharp-programming1
https://www.tutorialsteacher.com/linq/linq-case-insensitive-operation```