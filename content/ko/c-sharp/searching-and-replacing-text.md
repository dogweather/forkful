---
title:                "C#: 텍스트 검색 및 대체하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜
텍스트 검색 및 바꾸기를 하는 이유는 매우 간단합니다. 우리는 프로그래밍에서 자주 사용하는 특정한 단어나 구문을 쉽게 바꿀 수 있기 때문입니다. 이를테면, 우리가 제품에서 브랜드 이름을 바꾸거나, 오타를 수정하거나, 또는 특정한 포맷의 데이터를 다른 포맷으로 변환할 때 사용할 수 있습니다. 쉽게 말하면, 텍스트 검색 및 바꾸기는 매우 유용하고 강력한 도구입니다.

## 방법
텍스트 검색 및 바꾸기를 코딩으로 해결하는 방법은 여러 가지가 있지만, 이번 글에서는 C#에서 가장 일반적으로 사용하는 방법을 소개하고자 합니다. 먼저, 우리는 `Regex.Replace()` 함수를 사용하여 특정한 텍스트 패턴을 찾아서 다른 텍스트 패턴으로 바꿀 수 있습니다. 이 함수는 `Regex` 라이브러리에서 제공되며, 정규 표현식을 사용하여 매우 강력한 검색 기능을 제공합니다. 예를 들어, 우리가 문자열에서 모든 숫자를 찾아서 `#` 문자로 바꾸고 싶다고 합시다. 그러면 우리는 다음과 같은 코드를 사용할 수 있습니다.

```C#
using System.Text.RegularExpressions;

string content = "123 Love Programming";
string replaced = Regex.Replace(content, "[0-9]", "#");
Console.WriteLine(replaced); // 결과: ### Love Programming
```

위의 예제에서 우리는 `Regex.Replace()` 함수에 문자열에서 숫자를 찾는 정규 표현식 `[0-9]`과 바꿀 문자 `#`를 전달합니다. 이렇게 하면 숫자가 모두 `#`으로 바뀌게 됩니다.

또 다른 방법으로는, 우리는 `String.Replace()` 함수를 사용할 수도 있습니다. 이 함수는 정확한 문자열을 찾아서 다른 문자열로 바꿀 수 있습니다. 예를 들어, 우리가 위의 예제에서 `123`을 찾아서 `OneTwoThree`로 바꾸고 싶다고 합시다. 그러면 우리는 다음과 같은 코드를 사용할 수 있습니다.

```C#
string replaced = content.Replace("123", "OneTwoThree");
Console.WriteLine(replaced); // 결과: OneTwoThree Love Programming
```
위의 예제에서 우리는 `String.Replace()` 함수에 찾을 문자열과 바꿀 문자열을 전달하여 `123`을 `OneTwoThree`로 바꾸게 됩니다.

## 깊이있게 들어가기
텍스트 검색 및 바꾸기를 더욱 깊이있게 들어가보겠습니다. 우리는 지금까지 간단한 예제를 살펴보았지만, 실제로는 정규 표현식을 사용하여 매우 복잡한 텍스트 패턴도 찾고 바꿀 수 있습니다. 정규 표현식은 텍스트 검색 및 바꾸기를 더욱 강력하게 만들어주는 도구입니다. 이를테면, 우리는 이메일 주소를 검색하여 모두 `hidden`으로 바꾸거나, 주민등록번호를 검색하여 다른 포맷으로 바꾸는 등의 작업도 가능합니다.

또한, 우리는 텍스트를 바꿀 때 대소문자를 구분하지 않아도 됩니다. `Regex.Replace()` 함수와 `String