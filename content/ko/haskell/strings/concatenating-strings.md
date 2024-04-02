---
date: 2024-01-20 17:34:53.629060-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC5EC\uB7EC \uAC1C\uC758 \uBB38\
  \uC790\uC5F4\uC744 \uD558\uB098\uB85C \uC774\uC5B4 \uBD99\uC774\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uB370\uC774\uD130\uB97C \uC720\uC5F0\uD558\uAC8C \uC870\uC791\uD558\
  \uACE0, \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uBA54\uC2DC\uC9C0\uB97C \uC81C\uACF5\uD558\
  \uBA70, \uD30C\uC77C \uACBD\uB85C\uB97C \uAD6C\uC131\uD558\uB294 \uB4F1 \uD504\uB85C\
  \uADF8\uB798\uBA38\uC5D0\uAC8C \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.283822-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC5EC\uB7EC \uAC1C\uC758 \uBB38\uC790\
  \uC5F4\uC744 \uD558\uB098\uB85C \uC774\uC5B4 \uBD99\uC774\uB294 \uAC83\uC785\uB2C8\
  \uB2E4. \uB370\uC774\uD130\uB97C \uC720\uC5F0\uD558\uAC8C \uC870\uC791\uD558\uACE0\
  , \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uBA54\uC2DC\uC9C0\uB97C \uC81C\uACF5\uD558\uBA70\
  , \uD30C\uC77C \uACBD\uB85C\uB97C \uAD6C\uC131\uD558\uB294 \uB4F1 \uD504\uB85C\uADF8\
  \uB798\uBA38\uC5D0\uAC8C \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## What & Why? (무엇과 왜?)
문자열 연결은 여러 개의 문자열을 하나로 이어 붙이는 것입니다. 데이터를 유연하게 조작하고, 사용자에게 메시지를 제공하며, 파일 경로를 구성하는 등 프로그래머에게 필수적입니다.

## How to (어떻게 하나요?)
Haskell에서 문자열을 연결하는 기본 방법은 `(++)` 연산자를 사용하는 것입니다. 간단하죠!

```Haskell
main :: IO ()
main = do
  let hello = "안녕"
  let world = "하세요"
  putStrLn (hello ++ " " ++ world)
```
출력 결과:
```
안녕 하세요
```

또는 `concat` 함수를 사용해 문자열 리스트를 합칠 수도 있어요.

```Haskell
main :: IO ()
main = do
  let greetings = ["안녕", " ", "하세요"]
  putStrLn (concat greetings)
```
출력 결과는 위와 동일합니다.

## Deep Dive (심도있는 탐구)
문자열 연결은 오래된 개념입니다. Haskell에서 `(++)` 연산자는 리스트를 연결하는 함수이며, 문자열도 리스트이기 때문에 사용할 수 있습니다.

이외에도, `Data.Text` 모듈의 `append` 함수를 사용해 `Text` 타입의 문자열을 효율적으로 연결 가능합니다. `Text`는 큰 데이터와 많은 문자열 연산을 다룰 때 더 나은 성능을 제공합니다.

```Haskell
import qualified Data.Text as T

main :: IO ()
main = do
  let hello = T.pack "안녕"
  let world = T.pack "하세요"
  T.putStrLn (T.append hello (T.append " " world))
```

반면에, `Builder` 타입은 더 복잡한 문자열 연산에 사용됩니다. 더 효율적으로 문자열을 만들 수 있지만, 사용법이 조금 더 복잡합니다.

## See Also (더 보기)
- [Haskell Wiki on String](https://wiki.haskell.org/String)
- [Text library on Hackage](https://hackage.haskell.org/package/text)
- [Haskell Language official website](https://www.haskell.org/)

이 기사에서는 Haskell을 사용하여 문자열을 연결하는 기본적인 방법들과 조금 더 심도있는 내용을 다뤘습니다. 좀 더 자세한 정보를 원한다면 위의 링크에서 추가 정보를 찾아 보세요!
