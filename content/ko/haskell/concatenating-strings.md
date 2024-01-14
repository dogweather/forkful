---
title:    "Haskell: 문자열 연결하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜
스트링을 결합하는 것이 유용한 이유는 다양합니다. 모든 프로그래머는 스트링을 연결할 때 동일한 방법을 사용하고 이 방법을 익히는 것은 더 나은 코드 작성에 도움이 됩니다. 

## 어떻게
스트링을 결합하는 방법은 다양한데, 가장 일반적인 방법은 `++` 연산자를 사용하는 것입니다. 예시 코드는 다음과 같습니다.

```Haskell
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

main = do
  let str1 = "안녕하세요, "
  let str2 = "저는 Haskell입니다."
  let result = concatStrings str1 str2
  putStrLn result
```

위 코드의 결과는 "안녕하세요, 저는 Haskell입니다."가 됩니다. 또 다른 스트링을 추가하고 싶다면, 이 방법을 계속 사용하여 여러 스트링을 결합할 수 있습니다.

## 딥 다이브
스트링을 결합하는 또 다른 방법은 `concat` 함수를 사용하는 것입니다. `concat` 함수는 리스트 안의 모든 스트링을 결합하여 하나의 스트링으로 만들어 줍니다. 예시 코드는 다음과 같습니다.

```Haskell
concatStrings :: [String] -> String
concatStrings strs = concat strs

main = do
  let strings = ["안녕하세요, ", "저는 Haskell,", " 코딩을 좋아합니다."]
  let result = concatStrings strings
  putStrLn result
```

위 코드의 결과는 "안녕하세요, 저는 Haskell, 코딩을 좋아합니다."가 됩니다. `concat` 함수는 리스트 안의 항목 순서대로 스트링을 결합하므로, 주의해서 사용해야 합니다.

## 또 보기
- [Haskell 문자열 연결하기](https://stackoverflow.com/questions/26996131/haskell-concatenating-strings)
- [Haskell 기본 문법](https://ko.wikipedia.org/wiki/하스켈_(프로그래밍_언어))