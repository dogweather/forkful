---
title:                "Haskell: 문자열 대문자로 변환하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열의 첫 번째 문자를 대문자로 변환하는 것은 주로 사용자가 입력 받은 데이터를 정리하거나 출력 시 보기 좋게 하기 위해 사용됩니다.

# 방법

```Haskell
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```

위의 함수는 문자열을 입력으로 받아 첫 번째 문자를 대문자로 변환하여 반환하는 함수입니다. 이 함수를 사용하면 다양한 방식으로 문자열을 대문자로 변환할 수 있습니다.

```Haskell
capitalize "haskell" -- "Haskell"
capitalize "PYTHON" -- "PYTHON"
capitalize "Tutorial" -- "Tutorial"
```

위와 같이 입력한 문자열에서 첫 번째 문자를 대문자로 변환하는 것을 볼 수 있습니다.

# 깊이 파고들기

위에서 작성한 함수는 단순히 입력으로 받은 문자열의 첫 번째 문자를 대문자로 변환하는 것에 그칩니다. 하지만 해스켈에서는 더 많은 함수를 사용하여 문자열을 변환할 수 있습니다. 예를 들어, `map` 함수를 사용하면 입력으로 받은 모든 문자를 대문자로 변환할 수 있습니다.

```Haskell
capitalize :: String -> String
capitalize = map toUpper
```

또한, `foldr` 함수를 사용하면 입력으로 받은 문자열을 한 글자씩 순회하며 대문자로 변환할 수 있습니다.

```Haskell
capitalize :: String -> String
capitalize = foldr ((:) . toUpper) []
```

위에서 사용한 `map`과 `foldr` 함수는 해스켈에서 자주 사용되는 고차 함수이며, 문자열 변환 외에도 다양한 기능을 제공합니다.

# 관련 정보

해스켈에서 대문자로 변환하는 함수 외에도 문자열에 관련된 다양한 함수들이 있습니다. 관련된 정보를 더 알고 싶다면 아래의 링크들을 참고해 보세요.

## 더 알아보기

* [Haskell에서 대문자로 변환하기](https://www.codementor.io/@sheharyarn/haskell-basic-data-types-io-operations-and-basic-file-manipulation-vyx6gqdig)
* [Haskell 고차 함수 소개](https://guide.haskell.org/basic/hofs.html)
* [문자열 관련 함수들](https://hackage.haskell.org/package/base/docs/Data-String.html)