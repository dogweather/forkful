---
title:                "남겨진 부분 추출"
html_title:           "C#: 남겨진 부분 추출"
simple_title:         "남겨진 부분 추출"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

서브스트링 추출을 어떤 이유로 할까요? 서브스트링 추출은 C# 프로그래밍에서 중요한 기술 중 하나입니다. 변수나 문자열을 다룰 때, 특정 부분만 추출하여 사용하고 싶을 때가 있습니다. 이 때 서브스트링 추출이 유용하게 사용될 수 있습니다.

## 어떻게

서브스트링 추출은 C#에서 간단하게 할 수 있습니다. 아래 코드를 참고해봅시다.

```C#
string originalString = "안녕하세요, 반가워요!";
string subString = originalString.Substring(3, 5);

// output: "하세요"
```

위 예시에서 `originalString` 변수는 추출하고 싶은 전체 문자열을 가지고 있습니다. `Substring()` 메소드를 사용하여 `subString` 변수에 `originalString`에서 3번째 인덱스부터 5개의 문자를 추출하였습니다. 즉, 원하는 범위의 문자열을 추출할 수 있습니다.

더 발전된 예시를 보기 위해 아래 코드를 참고해봅시다.

```C#
string sentence = "I love C# programming!";
int index = sentence.IndexOf("C#");
string subString = sentence.Substring(index, 2);

// output: "C#"
```

여기서 `IndexOf()` 메소드를 사용하여 `sentence`에서 "C#"이라는 문자열이 처음 나타나는 인덱스를 찾았습니다. 그 다음 `Substring()` 메소드를 이용하여 해당 인덱스부터 2개의 문자를 추출하였습니다. 이처럼 `Substring()` 메소드는 인덱스를 기준으로 원하는 범위의 문자열을 추출할 수 있습니다.

## 딥 다이브

서브스트링 추출에는 여러 가지 다른 방법이 있습니다. `Substring()` 메소드뿐만 아니라 `Substr()` 메소드를 사용하는 방법, 또는 정규식을 이용하는 방법 등이 있습니다. 또한 `Substring()` 메소드에는 매개변수를 하나만 넣는 `Substring(int startIndex)`과 매개변수를 두 개 넣어서 `Substring(int startIndex, int length)`을 사용하는 방법이 있습니다.

서브스트링 추출은 문자열을 다루는 데 있어서 유용한 기능입니다. 여러 가지 방법을 활용하여 자신에게 가장 적합한 방법을 찾아보세요.

## 관련 링크

- [C# String.Substring Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Working with Strings in C# (C# Station)](https://csharp-station.com/Text.aspx)