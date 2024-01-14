---
title:    "Haskell: 패턴과 일치하는 문자 삭제"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

"## 왜"

삭제란 우리의 일상에서 흔히 접하게 되는 작업입니다. 하지만 때로는 특정한 패턴의 문자들을 삭제하는 작업이 필요하게 될 수도 있습니다. 이러한 작업을 해야하는 이유에는 다양한 이유가 있을 수 있지만, 가장 일반적인 이유는 데이터를 정리하거나 가독성을 높이기 위함입니다.

"## 어떻게 하나요"

이제 실제로 문자 패턴을 삭제하는 방법을 알아보겠습니다. Haskell에서는 데이터를 처리하기 위한 강력한 기능을 제공하는 함수들이 있습니다. 그 중에서도 가장 많이 사용되는 함수는 `filter`입니다. 이 함수는 리스트의 각 요소를 전달된 조건에 따라 선택적으로 포함시킵니다.

```Haskell
-- 예제 리스트
list = [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- 짝수만 남기고 홀수는 삭제하는 함수
filterEven list = filter (\x -> x `mod` 2 == 0) list

-- 사용 예시
filterEven list
-- 결과: [2, 4, 6, 8]
```

위 예제에서는 리스트의 각 요소를 `mod` 함수를 이용해 2로 나눈 나머지가 0인 경우만 남겨서 필터링하는 방법을 보여주었습니다. 하지만 문자열의 경우는 어떻게 할까요? 우리는 `filter` 함수와 함께 `Data.List`에서 제공하는 `isInfixOf` 함수를 이용해서 패턴을 검색하고 필터링할 수 있습니다.

```Haskell
-- 예제 문자열
str = "Hello World!"

-- "o" 문자를 포함한 부분만 남기고 나머지는 삭제하는 함수
filterPattern str = filter (isInfixOf "o") str

-- 사용 예시
filterPattern str
-- 결과: "Helloo"
```

위 예제에서는 문자열의 각 문자를 검사하고, "o" 문자를 포함하는 경우에만 남겨줍니다.

"## 깊게 파고들어보기"

Haskell에서 문자 패턴을 삭제하는 방법은 `filter` 함수를 이용해서 간단하게 구현할 수 있지만, 더 복잡한 패턴을 삭제하려면 어떻게 해야 할까요? 이를 위해서는 정규식을 사용해야 합니다. 정규식은 특정한 패턴을 표현하는 문자열 패턴입니다. Haskell에서는 정규식을 지원하는 `regex-base` 라이브러리를 제공하고 있습니다.

```Haskell
-- 예제 문자열
str = "Hello World!"

-- 2글자 이상의 모음을 모두 삭제하는 함수
deleteVowels str = subRegex (mkRegex "[aeiou]+") str ""

-- 사용 예시
deleteVowels str
-- 결과: "Hll Wrld!"
```

위 예제에서는 `mkRegex` 함수를 이용해 정규식을 생성하고, `subRegex` 함수를 이용해 해당 정규식에 맞는 부분을 삭제해주었습니다.

"See Also"

- `Data.List` 모듈 문서: https://hackage.haskell.org/package/base/docs/Data-List.html
- 정규식 사용 예제: https://wiki.haskell.org/Regular_expressions
- `regex-base` 라이브러리 문서: https://hackage.haskell.org/package/regex-base