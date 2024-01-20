---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 쓰나요?
문자열을 소문자로 변환한다는 것은 문자열에 있는 모든 대문자를 소문자로 바꾸는 과정입니다. 프로그래머들은 대소문자 구분 없이 텍스트를 비교하거나 정렬하기 위해 이 작업을 합니다.

## 어떻게 사용하나요?
```C#
string myString = "Hello World!";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString);
// 출력: hello world!
```
위의 C# 코드에서 `ToLower()` 함수를 사용하여 문자열 내의 모든 대문자를 소문자로 변환합니다.

## 깊이 분석
1) 역사적 맥락: 문자열을 소문자로 변환하는 기능은 프로그래밍 초기부터 사용되어 왔습니다. 초기 컴퓨터 시스템은 대소문자를 구분하지 않았지만, 현재의 시스템은 이를 구분하므로 이 작업이 필요합니다.

2) 대안: `ToLower()` 함수 외에도, 문자열에 대해 일련의 연산을 수행하여 대문자를 소문자로 변환할 수도 있지만, 거의 모든 상황에서 `ToLower()` 함수가 가장 효과적입니다.

3) 구현 세부 정보: `ToLower()` 함수는 문자열 내의 각 문자에 대해 Unicode 테이블을 참조하여 해당 소문자 버전을 찾습니다. 이는 문자가 어떤 언어든 상관없이 작동합니다.

## 참고 자료
- [Microsoft Docs: string.ToLower Method](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.tolower?view=net-5.0)
- [C# 문자열 함수 사용하기](https://www.csharpstudy.com/Tip/Tip-string.aspx)