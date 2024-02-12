---
title:                "문자열 대문자화"
aliases:
- /ko/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:31.025082-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
문자열의 첫 글자를 대문자로 변환하고 나머지 글자들은 소문자로 유지하는 것을 문자열의 첫 글자 대문자화라고 합니다. 프로그래머들은 출력 형식을 맞추거나, 텍스트의 문법적 정확성을 준수하거나, 생성된 데이터의 가독성을 향상시키기 위해 이 작업을 수행합니다.

## 어떻게:
Haskell에서는 별도의 제3자 라이브러리를 필요로 하지 않고 표준 라이브러리를 사용해 문자열을 대문자로 변환할 수 있습니다.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- 샘플 사용:
main = putStrLn $ capitalize "hello world"
```

출력:
```
Hello world
```

보다 복잡한 시나리오나 사용의 용이성을 위해서는 Haskell에서 효율적인 문자열 조작을 위해 인기 있는 `text`와 같은 제3자 라이브러리를 사용할 수 있습니다.

먼저, 프로젝트의 의존성에 `text`를 추가해야 합니다. 그런 다음, 다음과 같이 그것의 함수를 사용해 문자열을 대문자로 변환할 수 있습니다:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- text 라이브러리를 사용한 샘플 사용:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

출력:
```
Hello world
```

이 두 예제는 모두 제3자 라이브러리의 사용 유무와 상관없이 Haskell에서 문자열의 첫 글자를 대문자로 변환하는 간단하면서도 효과적인 방법을 보여줍니다.
