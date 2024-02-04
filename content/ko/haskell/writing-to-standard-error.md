---
title:                "표준 에러에 쓰기"
date:                  2024-02-03T19:33:23.426093-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
하스켈에서 표준 오류(stderr)로 쓰기는 프로그램이 일반 결과와 오류 메시지를 구분할 수 있게 합니다. 이는 문제 신호를 보내고 디버깅하는 데 중요하며, 프로그램의 주요 데이터나 결과를 담은 표준 출력(stdout)을 혼잡하게 하지 않습니다.

## 어떻게:
하스켈에서 `System.IO` 모듈을 사용한 base 라이브러리를 통해 stderr로 쓰기는 간단합니다. 아래는 기본 예제입니다:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "이것은 오류 메시지입니다."
```

이 프로그램의 stderr 출력은 다음과 같습니다:

```
이것은 오류 메시지입니다.
```

더 복잡한 애플리케이션에서 작업 중이거나 로깅(오류 포함)에 대한 더 나은 제어가 필요한 경우, 타사 라이브러리를 선택할 수 있습니다. 인기 있는 선택 중 하나는 하스켈 프로그래밍의 `mtl` 스타일과 통합하는 `monad-logger`입니다. 여기 `monad-logger`를 사용한 작은 코드 조각이 있습니다:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "monad-logger를 사용한 이 오류 메시지입니다."
```

실행할 때, `monad-logger` 버전도 마찬가지로 오류 메시지를 출력하지만, 구성에 따라 타임스탬프나 로그 레벨과 같은 추가 컨텍스트를 제공합니다:

```
[Error] monad-logger를 사용한 이 오류 메시지입니다.
```

두 방법 모두 stderr에 쓰기 위한 목적을 제공하며, 선택은 대체로 애플리케이션의 복잡성과 필요에 따라 달라집니다.
