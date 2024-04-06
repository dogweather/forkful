---
date: 2024-01-20 17:50:55.510898-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Haskell\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574\uC11C\
  \uB294 \uC77C\uBC18\uC801\uC73C\uB85C `printf` \uD568\uC218\uB098 \uD15C\uD50C\uB9BF\
  \ \uB9AC\uD130\uB7F4 \uAC19\uC740 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00\
  \ \uD544\uC694\uD569\uB2C8\uB2E4. \uC5EC\uAE30 `printf` \uC608\uC2DC\uAC00 \uC788\
  \uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.993836-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Haskell\uC5D0\uC11C \uBB38\uC790\
  \uC5F4 \uBCF4\uAC04\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574\uC11C\uB294 \uC77C\
  \uBC18\uC801\uC73C\uB85C `printf` \uD568\uC218\uB098 \uD15C\uD50C\uB9BF \uB9AC\uD130\
  \uB7F4 \uAC19\uC740 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\
  \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게 하나요?)
Haskell에서 문자열 보간을 사용하기 위해서는 일반적으로 `printf` 함수나 템플릿 리터럴 같은 외부 라이브러리가 필요합니다. 여기 `printf` 예시가 있습니다:

```Haskell
import Text.Printf (printf)

name :: String
name = "세상"

main :: IO ()
main = printf "안녕, %s!\n" name
```

실행 결과:

```
안녕, 세상!
```

## Deep Dive (심화 탐구)
문자열 보간은 다른 언어에서 흔한 기능으로, 초기 프로그래밍 언어에서부터 발전해 왔습니다. 하스켈에는 기본적으로 내장된 문자열 보간 기능이 없지만, `printf` 함수나 `text` 패키지와 같은 라이브러리를 사용할 수 있습니다. 

이들 중 `printf`는 C 언어의 영향을 받아 만들어진 함수로, 형식 지정자(format specifier)를 사용합니다. 또한, QuasiQuotes를 사용하여 템플릿 리터럴을 구현할 수 있는 `interpolate` 라이브러리 같은 여러 대안들이 있습니다.

```Haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate ( i )

main :: IO ()
main = putStrLn [i|안녕, #{name}!|]
```

이렇게 하면 변수를 문자열 안에 직접 삽입하여 더 자연스러운 문자열 작성이 가능합니다.

## See Also (더 보기)
- Haskell `printf` documentation: https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html
- `text` package on Hackage: https://hackage.haskell.org/package/text
- `interpolate` library: https://hackage.haskell.org/package/interpolate
- Stack Overflow discussion on string interpolation in Haskell: https://stackoverflow.com/questions/4978578/how-to-interpolate-variables-in-strings-in-haskell
