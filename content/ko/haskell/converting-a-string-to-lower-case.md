---
title:    "Haskell: 문자열을 소문자로 변환하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 대해 다른 언어와는 조금 다른 접근법을 제공하는 Haskell 언어에 대해 알아보려고 할 수 있습니다.

## 하기

먼저, 우리는 `Data.Char` 모듈을 불러옵니다. 이 모듈은 문자열 처리에 유용한 함수를 제공합니다. 그 중 하나가 `toLower` 함수입니다. 이 함수는 주어진 문자를 해당하는 소문자로 변환해줍니다. 예제를 살펴봅시다:

```Haskell
import Data.Char

toLower 'A'       -- 'a'
toLower 'h'       -- 'h'
toLower '5'       -- '5'
```

이제, 문자열을 소문자로 변환하는 방법을 살펴보겠습니다. 우선, `map` 함수를 사용하여 문자열의 각 문자를 `toLower` 함수에 적용합니다. 그 후, `concat` 함수를 사용하여 문자열을 합쳐줍니다. 예제를 살펴봅시다:

```Haskell
map toLower "HELLO"         -- "hello"
concat (map toLower "WORLD")  -- "world"
```

이 방법은 `toLower`를 한 문자씩 적용하므로 굉장히 간단하고 간결합니다.

## 딥 다이브

문자열을 소문자로 변환하는 것에 대해 좀 더 깊게 살펴보겠습니다. `toLower` 함수는 변환하는 대상이 알파벳 문자인 경우에만 따로 소문자로 변환해줍니다. 예를 들어, 코드 포인트가 65에서 90 사이에 있는 경우에만 변환해줍니다. 따라서, `toLower`는 아스키 코드만 다룰 수 있고 유니코드 문자열에는 적용할 수 없습니다. 하지만, `toLower`를 사용하기 전에 `Data.Text` 모듈의 `toLower` 함수를 사용하여 `Text` 타입으로 변환한 후 적용하면 유니코드 문자열에도 적용할 수 있습니다. 예제를 살펴봅시다:

```Haskell
import Data.Char (toLower)
import Data.Text (toLower)

toLower '가'  -- error
toLower 'A'    -- 'a'

toLower (Data.Text.toLower "한글")  -- "한글"
```

이렇게 하면 유니코드 문자열에 대해서도 정확하게 대소문자 변환을 할 수 있습니다.

## 참고

* [Haskell Data.Char 모듈 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
* [Haskell Data.Text 모듈 문서](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)
* [Haskell String 처리 방법](https://haskell.org/tutorial/string.html)