---
title:                "문자열의 길이 찾기"
html_title:           "Haskell: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 길이를 찾는 것은 매우 기본적이지만 유용한 작업입니다. 이를 통해 문자열의 길이를 알고 문자열을 조작하거나 처리하는 데에 도움이 됩니다. 문자열의 길이를 찾는 이유는 주어진 문자열이 얼마나 길고 어떤 처리를 할 수 있는지를 판단하기 위해서입니다. 많은 프로그래머들이 이 작업을 자주 수행하며, Haskell에서 이를 간단하고 효율적으로 수행할 수 있습니다.

## 방법:
Haskell에서 문자열의 길이를 찾는 것은 매우 간단합니다. `length`함수를 사용하여 아래와 같이 작성할 수 있습니다.

```Haskell
length "Hello, world!" 
```
출력:
```
13
```

또는 리스트와 같이 문자열이 아닌 다른 형식이라도 `length` 함수를 사용할 수 있습니다.

```Haskell
length [1, 2, 3, 4, 5]
```
출력:
```
5
```

## 깊이 파고들기:
문자열의 길이를 찾는 방법은 간단하지만, 이를 가능하게 만든 역사적인 배경에 대해 알아보는 것도 흥미로울 수 있습니다. 이 작업은 초기 프로그래밍 언어들에서도 매우 일반적인 작업이었고, 문자열의 길이를 찾는 함수는 거의 모든 프로그래밍 언어에서 존재합니다. 그리고 Haskell에서도 `length` 함수 외에도 다른 문자열 관련 함수들을 포함하고 있습니다. 예를 들어, `take` 함수를 사용하여 문자열에서 원하는 길이만큼의 문자만 추출할 수 있습니다.

또한, 다른 언어에서는 `length` 함수보다 더 알아보기 어려운 방법으로 문자열의 길이를 찾는 경우도 있습니다. 이는 Haskell의 강력한 함수형 프로그래밍 기능을 통해 간단하고 효율적으로 해결할 수 있습니다.

더 많은 정보를 원한다면, Haskell 공식 문서를 확인하시면 됩니다.

## 참조:
- [Haskell 공식 문서](https://www.haskell.org/documentation)