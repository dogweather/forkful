---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:31.025082-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC740 \uC18C\
  \uBB38\uC790\uB85C \uC720\uC9C0\uD558\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4\uC758\
  \ \uCCAB \uAE00\uC790 \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCD9C\uB825 \uD615\uC2DD\uC744 \uB9DE\uCD94\
  \uAC70\uB098, \uD14D\uC2A4\uD2B8\uC758 \uBB38\uBC95\uC801 \uC815\uD655\uC131\uC744\
  \ \uC900\uC218\uD558\uAC70\uB098, \uC0DD\uC131\uB41C \uB370\uC774\uD130\uC758 \uAC00\
  \uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.272094-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC740 \uC18C\uBB38\
  \uC790\uB85C \uC720\uC9C0\uD558\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4\uC758 \uCCAB\
  \ \uAE00\uC790 \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4."
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
