---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
부분 문자열 추출이란 주어진 문자열의 특정 부분을 가져오는 것을 말합니다. 이를 통해 프로그래머들은 불필요한 데이터를 제거하고, 필요한 정보만을 분리해 낼 수 있습니다.

## 어떻게 할 수 있을까: 
C#에서는 `Substring` 메소드를 이용해서 부분 문자열을 추출할 수 있습니다. 
필요한 정보만 선택적으로 가져오기 위한 예제를 살펴봅시다.

```C#
string original = "안녕하세요, C# 프로그래밍을 배우는 교육생 여러분!";
string part = original.Substring(10, 16);
Console.WriteLine(part);
```

여기서는 10번째 문자부터 16개의 문자를 가져왔으며, 출력 결과는 "C# 프로그래밍을 배우는" 입니다.

## 더 깊게 알아보기:
`Substring` 메소드는 .NET Framework 1.1부터 사용 가능하게 되었습니다. 이전에는 문자열 처리를 위해서 문자 단위로 직접 처리하는 것이 일반적이었습니다.

C# 에서는 `Substring` 외에도 `Split`, `Remove`, `Replace` 등의 메소드로도 문자열을 다룰 수 있습니다. 이 중에서 적절한 메소드를 선택해서 사용하면 됩니다.

`Substring` 메소드는 문자열의 복사본을 만들어서 처리하기 때문에, 원본 문자열은 변경되지 않습니다. 이를 `immutable` 하다고 합니다.

## 참고하면 좋을 사이트들: 
1. [Microsoft Official C# Documentation](https://docs.microsoft.com/ko-kr/dotnet/csharp/)
2. [C# Station Tutorial](https://www.csharp-station.com/tutorial)
3. [C# Substring Tutorial With Examples](https://stackify.com/csharp-substring/)
4. [Top C# Blogs](https://www.upguard.com/blog/top-50-csharp-blogs)