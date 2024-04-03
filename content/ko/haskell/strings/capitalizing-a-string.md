---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:31.025082-07:00
description: "\uC5B4\uB5BB\uAC8C: Haskell\uC5D0\uC11C\uB294 \uBCC4\uB3C4\uC758 \uC81C\
  3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD544\uC694\uB85C \uD558\uC9C0 \uC54A\
  \uACE0 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574 \uBB38\
  \uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.272094-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C\uB294 \uBCC4\uB3C4\uC758 \uC81C3\uC790 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uD544\uC694\uB85C \uD558\uC9C0 \uC54A\uACE0 \uD45C\uC900 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574 \uBB38\uC790\uC5F4\uC744 \uB300\
  \uBB38\uC790\uB85C \uBCC0\uD658\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
