---
title:    "C#: 문자열 대문자로 바꾸기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

본문에 대한 확인을 위해 단어나 문장의 첫글자만 대문자로 바꾸는 것은 필요한 경우가 있습니다. 예를 들어, 사용자의 이름을 첫글자만 대문자로 표시하거나, 제목을 보기 좋게 만들기 위해 첫글자를 대문자로 바꾸는 경우 등이 있습니다.

## 하는 법

C#에서 단어나 문장의 첫글자를 대문자로 바꾸는 방법은 간단합니다. 아래의 코드를 사용하면 됩니다.

```
string word = "hello";
string capitalizedWord = char.ToUpper(word[0]) + word.Substring(1);
Console.WriteLine(capitalizedWord);
```

위의 코드에서 우리는 우선 입력된 단어의 첫글자를 대문자로 바꾸고, 나머지 부분을 그대로 유지하는 방식을 사용하였습니다. 그리고 `String.Substring` 함수를 사용하여 첫글자를 제외한 나머지 부분을 병합하여 출력하였습니다.

위의 코드를 실행하면 `Hello`라는 결과값이 출력됩니다.

## 심층 분석

위의 예제 코드는 우리가 비교적 쉽게 이해할 수 있도록 간단하게 작성되었습니다. 하지만 실제로는 `String.ToUpper` 함수를 사용해도 같은 결과를 얻을 수 있습니다. 우선 `String.ToUpper` 함수는 모든 문자를 대문자로 변환하는 것이 아니라, 소문자를 대문자로 변환하는 역할을 합니다. 그래서 위의 예제에서는 문자 하나만 변환하기 위해 `char.ToUpper` 함수를 사용하였습니다.

또한 위의 코드에서는 단어의 첫글자만 대문자로 변환하였지만, 만약 모든 단어의 첫글자를 대문자로 바꾸고 싶다면 `String.ToTitleCase` 함수를 사용할 수 있습니다.

## 관련 자료

- [C# Tutorial - Strings](https://www.c-sharpcorner.com/article/c-sharp-corner/)
- [C# String Methods](https://www.tutorialsteacher.com/csharp/csharp-string-methods)