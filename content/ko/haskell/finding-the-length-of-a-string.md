---
title:    "Haskell: 문자열의 길이 찾기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜


문자열의 길이를 찾는 것은 Haskell 프로그래밍 언어를 학습하고 개발하는 데 중요한 부분입니다. 이 기능을 이해하면 다양한 유형의 문자열 데이터를 처리할 수 있으며, 프로그래밍 능력을 향상시킬 수 있습니다.

## 어떻게

"```Haskell
length :: [a] -> Int
```"

Haskell에서 문자열의 길이를 찾는 함수는 "length" 함수입니다. 이 함수는 문자열을 입력으로 받아 해당 문자열의 길이를 정수로 반환합니다. 예를 들어, "Hello, world!"의 길이는 "12"가 됩니다.

"```Haskell
length "Hello, world!" 
-- 출력 결과: 12 
```"

또 다른 예로서, 이번에는 공백이 포함된 문자열을 사용해 보겠습니다.

"```Haskell
length "안녕하세요, 세계!" 
-- 출력 결과: 11 
```"

또 다양한 유형의 문자열을 사용해 보면서 "length" 함수가 어떻게 작동하는지 살펴보세요.

## 심층 분석 

"length" 함수는 입력으로 "a" 타입의 리스트를 요구합니다. Haskell에서는 문자열을 단순히 문자의 리스트로 처리하기 때문에, "length" 함수는 문자열을 입력 받아 해당 문자열의 길이를 반환합니다. 이를테면, 다음과 같은 방식으로 리스트의 길이를 계산할 수 있습니다.

"```Haskell
length [1,2,3,4] -- 리스트의 길이는 4가 됩니다.
length ['H', 'e', 'l', 'l', 'o'] -- 문자열 "Hello"의 길이는 5가 됩니다.
```"

또한, 문자열의 길이를 계산할 때 재귀 함수를 사용할 수도 있습니다. 이를테면 다음과 같이 구현할 수 있습니다.

"```Haskell
strLength :: String -> Int
strLength [] = 0 -- 공백 문자열인 경우 길이는 0
strLength (x:xs) = 1 + strLength xs -- 문자열을 순회하면서 길이를 계산
```"

위의 함수는 입력으로 받은 문자열의 첫 번째 요소를 제외하고 계속해서 재귀적으로 호출하여 마지막에는 더이상 요소가 없는 경우 0을 반환합니다. 따라서 문자열의 길이를 계산할 수 있습니다. 

## 관련 정보 

다른 Haskell 기능에 대한 정보를 얻으려면 다음 링크를 참조하세요.

* [Haskell 공식 문서](https://www.haskell.org/documentation/)
* [Haskell 커뮤니티 포럼](https://www.haskell.org/community/)
* [Haskell 온라인 강좌](http://learnyouahaskell.com/)
* [Haskell 무료 책](https://haskellbook.com/)