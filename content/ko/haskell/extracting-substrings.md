---
title:                "Haskell: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜 substring 추출에 대해 알아볼까?

substring 추출은 문자열을 조작하는 중요한 기술 중 하나입니다. 이를 이용하여 특정 단어나 문장을 추출하거나, 문자열 검색에서 유용하게 사용할 수 있습니다. 또한 substring 추출은 프로그램의 성능을 향상시키는 데에도 도움이 됩니다.

## 추출하는 방법

```Haskell
substring :: Int -> Int -> String -> String
substring start len str = drop start (take (start + len) str)
```

위의 예시 코드는 `substring` 함수를 정의하는 방법입니다. 이 함수는 세 개의 인자를 받습니다. 첫 번째 인자는 시작 위치, 두 번째 인자는 추출할 길이, 세 번째 인자는 추출할 문자열입니다. 이 함수는 `drop` 함수와 `take` 함수를 이용하여 문자열을 추출하고, 추출된 문자열을 반환합니다.

```Haskell
do
    putStrLn "Input a string:"
    str <- getLine
    let start = 3
    let len = 3
    let result = substring start len str
    putStrLn result
```

위의 예시 코드는 `substring` 함수를 사용하는 방법입니다. 사용자로부터 문자열을 입력받은 후, `substring` 함수를 이용하여 시작 위치 3에서 길이 3만큼의 문자열을 추출하고, 추출된 문자열을 출력합니다.

**출력:**

`Hello World` 를 입력하면 `lo ` 가 출력됩니다.

## 깊이 들어가보기

`substring` 함수를 만드는 방법은 여러 가지가 있지만 위에서 소개한 방식은 가장 간단하고 효율적인 방법 중 하나입니다. 이 함수를 이용하여 문자열을 추출하면 기존 문자열의 메모리 주소를 공유하므로, 성능적인 측면에서도 효율적입니다.

또한 `substring` 함수는 Haskell의 강력한 장점 중 하나인 커링(currying) 기법을 이용하여 만들어져 있습니다. 이를 이용하면 이 함수를 여러 번 호출하거나, 인자 중 일부를 미리 지정하여 새로운 함수를 만들 수 있습니다.

## 더 알아보기

**## 지켜야 할 사항**

[Haskell Official Documentation](https://downloads.haskell.org/~ghc/6.12.1/docs/html/libraries/base-4.2.0.1/Data-List.html#v%3Asubstring)

**## 다른 유용한 함수들**

[drop](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:drop), [take](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:take), [!!](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:-33-33), [words](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:words)