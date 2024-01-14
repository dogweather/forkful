---
title:    "Haskell: 부분 문자열 추출"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# 왜

문자열을 추출하는 것에 대해 많은 질문이 있을 수 있지만, 우리는 이를 해결하기 위해 이 작은 가이드를 제공할 것입니다. 이 기술은 몇 가지 장점이 있습니다. 

예를 들어, 어떤 경우에는 문자열 전체가 아닌 일부 문자열만 필요할 수 있고, 이를 추출하는 작업은 아주 유용합니다. 또한 다양한 데이터를 처리해야 할 때 문자열 추출 작업이 필요할 수도 있습니다. 문자열 추출은 다양한 상황에서 유용하게 사용할 수 있으며, 이를 잘 활용하면 시간과 노력을 절약할 수 있습니다.

# 사용 방법

Haskell에서 문자열을 추출하는 방법은 간단합니다. 아래의 코드 블록을 따라하면서 살펴보겠습니다.

```Haskell
-- 모듈을 불러옵니다.
import Data.List (isPrefixOf, drop, take)

-- 추출할 문자열입니다.
myString = "안녕하세요! 이것은 샘플 문자열입니다."

-- 첫 번째 단어를 추출합니다.
firstWord = takeWhile (not . isSpace) myString

-- 두 번째 단어부터 추출합니다.
secondWord = dropWhile (not . isSpace) myString
```

위의 코드 블록에서 우리는 `Data.List` 모듈에서 `isPrefixOf`, `drop`, `take` 함수를 불러와 사용했습니다. 먼저 `isPrefixOf`는 문자열이 해당 단어로 시작하는지를 확인하는 함수입니다. `takeWhile` 함수를 이용해 해당 단어가 시작하는 인덱스까지 추출했습니다. 그리고 `dropWhile` 함수를 이용해 해당 단어 이후부터 추출했습니다. 이렇게 함으로써 우리는 첫 번째 단어와 두 번째 단어를 추출할 수 있게 됩니다.

## 깊게 들어가기

또 다른 예제로 아래의 코드 블록을 살펴보겠습니다.

```Haskell
-- 추출할 문자열입니다.
myString = "안녕하세요! 이것은 샘플 문자열입니다."

-- 6번째 인덱스부터 10번째 인덱스까지 추출합니다.
subString = take 5 (drop 5 myString)
```

위의 코드 블록에서는 `take`와 `drop` 함수를 이용하여 원하는 범위의 문자열을 추출하였습니다. 이렇게 함으로써 우리는 더욱 복잡한 문자열 추출 작업을 할 수 있습니다. 

# 더 알아보기

문자열 추출에 대해 더 많은 정보를 알고 싶다면 아래의 링크들을 참고해보세요!

- [Haskell strings - Learn You a Haskell](https://learnyouahaskell.com/starting-out#strings)
- [Data.List - Haskell](https://hackage.haskell.org/package/base/docs/Data-List.html) 
- [String manipulation in Haskell - Stack Overflow](https://stackoverflow.com/questions/13769346/string-manipulation-in-haskell) 

# 참고

- [Markdown](https://www.markdownguide.org/basic-syntax/)