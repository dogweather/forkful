---
title:                "C#: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열과 관련하여 작업하는 프로그래머라면 입력된 문자열에서 일부분을 추출해야 할 때가 있습니다. 이를 "substring 추출"이라고 합니다. 이 기술을 알아두면 문자열을 더욱 효율적으로 다룰 수 있습니다.

## 어떻게

```C#
string word = "안녕하세요!";
string substring = word.Substring(0, 2); // "안녕"
Console.WriteLine(substring);
```

위의 예시 코드에서 우리는 변수 `word`에 문자열 "안녕하세요!"를 저장합니다. 그리고 `Substring()` 메소드를 사용하여 `word` 변수에서 인덱스 0에서부터 길이가 2인 substring을 추출합니다. 즉, 위의 예시 코드에서는 "안녕"이라는 substring을 추출하고 이를 콘솔에 출력합니다.

```C#
string sentence = "저는 C# 프로그래머입니다.";
string name = sentence.Substring(2, 2); // "C#"
Console.WriteLine(name);
```

위의 예시 코드에서는 더욱 다양한 패턴의 문자열에서 substring을 추출하는 방법을 보여줍니다. `sentence` 변수에 저장된 문자열에서 인덱스 2부터 길이가 2인 substring을 추출하였고 이를 `name` 변수에 저장하였습니다. 콘솔에 출력된 결과는 "C#"입니다.

## 딥 다이브

결과적으로, `Substring()` 메소드를 사용하여 문자열에서 일부분을 추출하는 방법은 매우 간단합니다. 그러나 이 기술을 더욱 자세히 살펴본다면 더 다양한 방법을 발견할 수 있습니다. 예를 들어, `Substring()` 메소드를 중첩하여 더 복잡한 패턴의 substring을 추출할 수도 있고 다양한 파라미터를 설정하여 더욱 다양한 결과를 얻을 수도 있습니다.

## 참고

- [참조 문서 - C#의 Substring() 메소드](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.substring?view=net-5.0)
- [정규표현식을 이용한 문자열 추출 기법](https://www.joinc.co.kr/w/Site/Java/StringExtractRegularExpression)