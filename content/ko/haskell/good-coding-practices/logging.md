---
date: 2024-01-26 01:07:50.650946-07:00
description: "\uBC29\uBC95: Haskell\uC5D0\uC11C \uB85C\uAE45\uC740 `monad-logger`\
  \ \uB610\uB294 `hslogger`\uC640 \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\
  \uAE30 `monad-logger`\uB97C \uC0AC\uC6A9\uD55C \uAC04\uB2E8\uD55C \uC608\uC2DC\uAC00\
  \ \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.304760-06:00'
model: gpt-4-1106-preview
summary: "Haskell\uC5D0\uC11C \uB85C\uAE45\uC740 `monad-logger` \uB610\uB294 `hslogger`\uC640\
  \ \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uAD6C\
  \uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 방법:
Haskell에서 로깅은 `monad-logger` 또는 `hslogger`와 같은 라이브러리를 사용하여 구현할 수 있습니다. 여기 `monad-logger`를 사용한 간단한 예시가 있습니다:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "응용 프로그램 시작..."
    liftIO $ putStrLn "중요한 작업을 수행 중..."
    logErrorN "이런! 문제가 발생했습니다."

main :: IO ()
main = runStdoutLoggingT logExample

{- 샘플 출력
[정보] 응용 프로그램 시작...
중요한 작업을 수행 중...
[오류] 이런! 문제가 발생했습니다.
-}
```

이 간단한 예시는 런타임에 무슨 일이 일어나고 있는지 파악하기 위해 코드 곳곳에 로깅 구문을 삽입하는 방법을 보여줍니다. `logInfoN`과 `logErrorN`은 각각 정보와 오류 메시지를 로깅하는 데 사용됩니다.

## 심화 학습:
로깅은 단순한 print문에서부터 정교한 로깅 프레임워크에 이르기까지 오랜 길을 걸어왔습니다. 과거에는 로그가 콘솔이나 파일로의 텍스트 출력에 불과했지만, 이제는 다양한 도구로 파싱하고 분석할 수 있는 구조화된 데이터를 포함합니다.

Haskell에서 로깅은 로그 액션을 명백하게 전달하거나 로거가 계산을 통해 암시적으로 스레딩되는 불순한 모나딕 컨텍스트를 사용하는 순수 함수형 스타일로 수행될 수 있습니다.

예를 들어 `hslogger` 라이브러리는 `monad-logger`에 비해 전통적이고 변경 가능합니다. `monad-logger`는 모나드 스택과의 통합을 제공하며 출력 형식과 제어 면에서 더 많은 유연성을 제공합니다. 두 라이브러리 모두 로그 수준을 설정할 수 있으며, 이를 통해 중요도에 따라 로그 메시지를 필터링할 수 있습니다. 로그 수준에는 디버그, 인포, 공지, 경고, 오류, 중요, 경보, 긴급 등이 포함됩니다.

Haskell의 로깅 접근 방식은 종종 타입 안전성과 순수성에 대한 강조와 일치합니다. Haskell의 견고한 에러 핸들링 능력 덕분에 로깅이 실패하더라도 주 어플리케이션의 충돌로 이어지지 않도록 로그를 처리할 수 있습니다.

## 참조:
- [`monad-logger` Hackage상의 문서](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` 패키지 Hackage](https://hackage.haskell.org/package/hslogger)
- [실제 세계의 Haskell, 19장, 에러 처리에 대하여](http://book.realworldhaskell.org/read/error-handling.html)
- [Haskell을 위한 로깅 파사드 (log-base)](https://hackage.haskell.org/package/log-base)
