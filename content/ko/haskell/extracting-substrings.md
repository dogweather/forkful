---
title:    "Haskell: 부분 문자열 추출하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜

문자열에서 부분 문자열을 추출하는 것은 프로그래밍에서 자주 사용되는 작업입니다. 이를 통해 우리는 문자열의 특정 부분을 정확하게 가져올 수 있고, 이를 다른 작업에 사용할 수 있습니다.

## 추출하는 방법

Haskell에서 문자열에서 부분 문자열을 추출하는 것은 간단합니다. 예를 들어, 문자열 "안녕하세요"에서 "안녕"이라는 부분 문자열을 추출해 보겠습니다.

```Haskell
substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

main = do
  print (substring 0 2 "안녕하세요")
```
이 코드는 "안녕"이라는 부분 문자열을 추출하고 출력하는 간단한 예제입니다. ```substring``` 함수는 시작 지점과 끝 지점을 매개변수로 받고, ```take``` 함수를 사용하여 해당 범위의 문자를 가져온 후, ```drop``` 함수를 사용하여 이전 문자를 제거하는 방식으로 부분 문자열을 추출합니다. 이 코드를 실행하면 "안녕"이라는 문자열이 출력됩니다.

## 깊이 들어가기

보다 복잡한 예제를 살펴보겠습니다. 이번에는 문자열 "친구들이 재미있게 놀고 있었다"에서 "재미있게 놀고 있었다"라는 부분 문자열을 추출해 보겠습니다.

```Haskell
substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

main = do
  let sentence = "친구들이 재미있게 놀고 있었다"
  let start = 3
  let end = 14
  print (substring start end sentence)
```

이번에는 시작 지점과 끝 지점을 변수로 지정하여 문자열을 추출하는 예제입니다. 이렇게 함으로써 우리는 같은 코드를 여러 번 사용하여 쉽게 부분 문자열을 추출할 수 있습니다.

# 참고

- [Haskell 문자열 추출하기](https://www.codewars.com/kata/576b9f21b59692368500006b)
- [Haskell 함수 문서](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#g:12)
- [Haskell 문자열 함수](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)