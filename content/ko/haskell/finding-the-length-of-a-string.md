---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요할까?

문자열의 길이를 찾는 것은 문자열에 포함된 문자의 수를 계산하는 것을 의미합니다. 이는 다양한 프로그래밍 작업에서 꼭 필요한 기능이며, 기본적으로 모든 컴퓨터 언어에서 제공되는 핵심 기능입니다.

## 어떻게 하나요?

Haskell에서는 `length` 함수를 통해 문자열의 길이를 알아낼 수 있습니다.

```Haskell
main = do
    let myString = "안녕하세요"
    print (length myString)
```

이 코드를 실행하면 "5" 를 반환합니다.

## 깊이 들여다보기

### 역사

`length` 함수는 Haskell의 초기 버전부터 존재하고 있습니다. 이는 본래 더 복잡한 작업들을 단순화하기 위해 만들어진 함수입니다.

### 대안

`length` 함수는 전체 문자열을 탐색해야 하기 때문에 규모가 큰 문자열에 대해 느릴 수 있습니다. 이를 대안으로 `Data.Text.length` 함수가 제공되며, 이는 원시 문자열보다 더 효율적입니다.

### 내부 구현

`length` 함수는 리스트의 길이를 계산하는 함수입니다. Haskell에서 문자열은 사실 문자의 리스트이기 때문에 `length` 함수는 문자열의 길이를 찾는데에도 사용할 수 있습니다. 이 함수의 구현은 꽤 간단합니다.

```Haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

## 참고자료

더 많은 정보를 얻고자 할 때, 아래의 링크를 확인해 보세요.

- [Haskell Documentation for length](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:length)
- [StackOverflow Answer about alternatives](https://stackoverflow.com/questions/2026912/quick-way-to-find-the-length-length-of-a-text-string-in-haskell)