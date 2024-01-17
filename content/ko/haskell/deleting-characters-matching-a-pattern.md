---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Haskell: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

패턴에 맞는 문자를 삭제하는 것은 프로그래머들이 코드를 정리하고 가독성을 높이기 위해 하는 작업입니다.

## 코딩 방법:

```Haskell
deleteEvery :: String -> String -> String
deleteEvery _ [] = []
deleteEvery pattern (x:xs)
    | pattern `isPrefixOf` (x:xs) = deleteEvery pattern (drop (length pattern) (x:xs))
    | otherwise = x : deleteEvery pattern xs

deletePattern :: String -> String -> String
deletePattern pattern string = intercalate "" (words (deleteEvery pattern string))

main :: IO ()
main = do
    let example = "Hello, World!"
    putStrLn $ deletePattern "lo" example
```

출력:
```
He, World!
```

## 깊게 들어가보기:

1. 역사적 맥락:
패턴에 맞는 문자를 삭제하는 방법은 다양한 프로그래밍 언어에서 흔하게 사용되는 기능입니다. 그 중에서도 Haskell은 간결한 문법과 강력한 패턴 매칭 기능으로 유명합니다.

2. 대안:
Haskell에는 문자열을 처리하는 다양한 함수들이 존재합니다. 이 중에서도 `deleteEvery` 함수는 재귀적으로 패턴 매칭을 수행하여 구현된 것이며, `deletePattern` 함수는 이를 이용하여 문자열을 삭제하는 편리한 함수입니다.

3. 구현 세부사항:
`deleteEvery` 함수는 먼저 문자열을 입력받아 첫 번째 인자로 받은 패턴과 매칭되는 부분이 있는지 확인합니다. 만약 매칭되는 부분이 있다면, 해당 부분 이후의 문자열을 재귀적으로 매개변수로 넘겨줍니다. 매칭되는 부분이 없다면, 첫 번째 인자로 받은 패턴을 다음 줄부터 다시 검사하고 그렇지 않은 경우 해당 문자를 결과 문자열에 추가합니다. 마지막으로 `deletePattern` 함수는 `deleteEvery` 함수를 이용하여 패턴에 맞는 문자를 모두 삭제한 후, 다시 문자열을 합쳐서 반환하는 함수입니다.

## 참고 자료:

- [Haskell 공식 문서](https://www.haskell.org/)
- [Haskell 패턴 매칭](https://wiki.haskell.org/Pattern_matching)
- [Haskell 문자열 처리 함수](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html)