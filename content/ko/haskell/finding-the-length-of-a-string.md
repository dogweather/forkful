---
title:                "Haskell: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜 
문자열의 길이를 찾는 것에 대해 알아보는 이유는 무엇일까요? 일반적으로 프로그래밍에서 문자열은 매우 중요한 데이터 타입으로 사용됩니다. 따라서 문자열의 길이를 알아내는 것은 많은 작업을 하는 데 도움이 됩니다.

## 어떻게 
우리는 Haskell을 사용하여 간단한 함수를 작성해보겠습니다. 이 함수는 문자열의 길이를 알아내는 기능을 합니다. 이를 위해 내장 함수인 "length"를 사용할 것입니다. 아래의 예제를 따라 해보세요.

```Haskell
length "안녕하세요" 
```
```Haskell
6
```

위의 예제에서 볼 수 있듯이 "안녕하세요"라는 문자열의 길이는 6이 됩니다. 만약 다른 문자열의 길이를 알고 싶다면, 자유롭게 다른 문자열을 대입해보세요.

```Haskell
length "반가워요" 
```
```Haskell
5
```

위의 예제에서도 볼 수 있듯이 "반가워요"라는 문자열의 길이는 5가 됩니다.

## 딥 다이브 
위의 예제에서 우리는 "length"라는 함수를 간단히 사용하여 문자열의 길이를 알아냈습니다. 하지만 조금 더 깊게 들어가보면, 이 함수는 실제로 어떻게 작동하는 걸까요?

Haskell에서 문자열은 리스트로 표현됩니다. 따라서 "length" 함수는 리스트의 길이를 알아내는 함수입니다. 그리고 문자열은 각각의 문자로 구성되어 있기 때문에, 문자열의 길이를 알아내는 것은 결국 문자의 개수를 세는 것과 같습니다.

## 봐보세요 
Haskell에서 문자열의 길이를 알아내는 것은 프로그래밍에서 매우 중요한 작업 중 하나입니다. 이를 더 자세히 다루고 싶다면 아래의 링크를 참조해보세요.

- [Haskell Language Tutorials](https://www.tutorialspoint.com/haskell/)
- [String Functions in Haskell](https://www.geeksforgeeks.org/string-functions-in-haskell/)
- [Haskell String Length](https://www.educba.com/haskell-string-length/)