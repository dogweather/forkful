---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:32.291984-07:00
description: "\uBC29\uBC95: Haskell\uC740 \uADF8\uAC83\uC758 \uAE30\uBCF8 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD574 \uB514\uB809\uD1A0\uB9AC \uC874\uC7AC \uC5EC\
  \uBD80\uB97C \uD655\uC778\uD558\uB294 \uC9C1\uAD00\uC801\uC778 \uBC29\uBC95\uC744\
  \ \uC81C\uACF5\uD558\uBA70, \uC8FC\uB85C `System.Directory` \uBAA8\uB4C8\uC744 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4. \uAE30\uBCF8 \uC608\uC81C\uB97C \uC0B4\uD3B4\uBD05\uC2DC\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.316509-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC740 \uADF8\uAC83\uC758 \uAE30\uBCF8 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uD1B5\uD574 \uB514\uB809\uD1A0\uB9AC \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\
  \uC778\uD558\uB294 \uC9C1\uAD00\uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\
  \uBA70, \uC8FC\uB85C `System.Directory` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Haskell은 그것의 기본 라이브러리를 통해 디렉토리 존재 여부를 확인하는 직관적인 방법을 제공하며, 주로 `System.Directory` 모듈을 사용합니다. 기본 예제를 살펴봅시다:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "디렉토리가 존재합니까? " ++ show exists
```

디렉토리가 존재하는지 여부에 따라 달라지는 샘플 출력:

```
디렉토리가 존재합니까? True
```
또는:
```
디렉토리가 존재합니까? False
```

더 복잡한 시나리오나 추가 기능을 위해, 파일 경로를 보다 추상적으로 처리하고 조작하는 데 인기 있는 제3자 라이브러리인 `filepath`의 사용을 고려할 수 있습니다. 하지만, 단순히 디렉토리 존재 여부를 확인하는 목적으로는 기본 라이브러리의 `System.Directory`가 충분하고 효율적입니다.

파일 시스템을 다루는 것은 플랫폼에 따라 달라질 수 있으며, Haskell의 접근 방식은 이러한 차이 중 일부를 추상화하려고 합니다. 예상된 동작을 보장하기 위해 타겟 시스템에서 파일 작업을 항상 테스트하세요.
