---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:07.055791-07:00
description: "\uBC29\uBC95: Haskell\uC758 \uD45C\uC900 Prelude\uB294 `System.IO` \uBAA8\
  \uB4C8\uC758 `writeFile` \uBC0F `appendFile` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uD30C\uC77C\uC5D0 \uC4F0\uB294 \uAE30\uBCF8 \uC9C0\uC6D0\uC744 \uC81C\uACF5\
  \uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC0C8 \uD30C\uC77C\uC744 \uC0DD\uC131(\uB610\
  \uB294 \uAE30\uC874 \uD30C\uC77C\uC744 \uB36E\uC5B4\uC4F0\uAE30)\uD55C \uB2E4\uC74C\
  \ \uD30C\uC77C\uC5D0 \uD14D\uC2A4\uD2B8\uB97C \uCD94\uAC00\uD558\uB294 \uAE30\uBCF8\
  \ \uC608\uC81C\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.322268-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC758 \uD45C\uC900 Prelude\uB294 `System.IO` \uBAA8\uB4C8\uC758\
  \ `writeFile` \uBC0F `appendFile` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD30C\
  \uC77C\uC5D0 \uC4F0\uB294 \uAE30\uBCF8 \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Haskell의 표준 Prelude는 `System.IO` 모듈의 `writeFile` 및 `appendFile` 함수를 사용하여 파일에 쓰는 기본 지원을 제공합니다. 다음은 새 파일을 생성(또는 기존 파일을 덮어쓰기)한 다음 파일에 텍스트를 추가하는 기본 예제입니다.

```haskell
import System.IO

-- 파일에 쓰기, 존재할 경우 덮어쓰기
main :: IO ()
main = do
  writeFile "example.txt" "첫 번째 줄입니다.\n"
  appendFile "example.txt" "두 번째 줄입니다.\n"
```

이 프로그램을 실행하면 `example.txt`을 생성(또는 비우기)하고 "첫 번째 줄입니다."라고 쓴 다음 다음 줄에 "두 번째 줄입니다."라고 씁니다.

보다 고급 파일 처리를 위해 Haskell 프로그래머들은 종종 효율적인 문자열 처리를 위한 `text` 패키지와 이진 데이터 처리를 위한 `bytestring` 패키지로 전환합니다. 다음은 파일 IO를 위해 `text` 패키지를 사용하는 방법입니다:

먼저, 프로젝트 의존성에 `text`를 추가해야 합니다. 그런 다음, 다음과 같이 사용할 수 있습니다:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- text 패키지를 사용하여 파일에 쓰기
main :: IO ()
main = do
  let content = T.pack "더 나은 성능을 위해 text 패키지 사용.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "두 번째 줄 추가.\n"
```

이 스니펫에서, `T.pack`은 일반 `String`을 더 효율적인 `Text` 타입으로 변환합니다. `TIO.writeFile` 과 `TIO.appendFile`은 각각 파일에 쓰고 추가하는 `text`의 대응하는 함수입니다.

이 코드를 실행하면 `textExample.txt`라는 파일이 두 줄의 텍스트와 함께 생성되어, 유니코드 텍스트를 처리하는 능력과 성능 면에서 향상된 고급 `text` 라이브러리를 사용하여 생성 및 추가 기능을 보여줍니다.
