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

## 왜

문자열의 길이를 찾는 것은 현재 많이 사용되는 프로그래밍 기술이며, 여러분의 코드에 필수적인 요소입니다.

## 방법

문자열의 길이를 찾는 방법은 매우 간단합니다. 아래의 코드를 따라 해보세요.

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

main = do
  let str = "Hello World"
  putStrLn $ show (length str)
```

위의 예제 코드는 문자열 "Hello World"의 길이를 찾는 방법을 보여줍니다. 코드를 실행하면 콘솔에 11이 출력됩니다.

## 딥 다이브

문자열의 길이를 찾는 방법은 위의 예제 코드와 같이 재귀적으로 구현되어 있습니다. 예를 들어, "Hello World"의 길이를 찾을 때는 문자열을 하나씩 나누어서 길이를 찾게 됩니다. 이런 방식으로 길이를 찾는 데는 n만큼의 시간이 걸리기 때문에 선형 시간 알고리즘이라고 할 수 있습니다.

여러분이 작성하는 코드에서 문자열의 길이를 자주 사용하게 될 것이므로, 이를 빠르게 구현하는 습관을 기르는 것이 좋습니다.

## 참고

- [Haskell 공식 사이트](https://www.haskell.org/)
- [Haskell 예제 코드](https://www.programiz.com/haskell-programming/examples)
- [Haskell 책 추천](https://medium.com/@_geraud/haskell-book-reviews-3c608474e3ea)