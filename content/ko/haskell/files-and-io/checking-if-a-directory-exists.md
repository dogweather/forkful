---
aliases:
- /ko/haskell/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:32.291984-07:00
description: "\uB514\uB809\uD1A0\uB9AC \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\
  \uD558\uB294 \uAC83\uC740 \uB9CE\uC740 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\
  \uC5D0\uC11C \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC73C\uB85C, \uB514\uB809\uD1A0\
  \uB9AC \uAD6C\uC870\uC758 \uC874\uC7AC \uC5EC\uBD80\uC5D0 \uB530\uB77C \uC870\uAC74\
  \uBD80 \uC791\uC5C5\uC744 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD30C\uC77C\
  \ \uC870\uC791, \uC790\uB3D9 \uC2A4\uD06C\uB9BD\uD2B8, \uADF8\uB9AC\uACE0 \uC18C\
  \uD504\uD2B8\uC6E8\uC5B4\uC758 \uCD08\uAE30 \uC124\uC815 \uACFC\uC815\uC5D0\uC11C\
  \ \uD544\uC694\uD55C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC7A5\uC18C\uC5D0 \uC788\uB294\
  \uC9C0 \uD655\uC778\uD558\uAC70\uB098 \uB514\uB809\uD1A0\uB9AC\uB97C \uC911\uBCF5\
  \uC73C\uB85C\u2026"
lastmod: 2024-02-18 23:09:06.309525
model: gpt-4-0125-preview
summary: "\uB514\uB809\uD1A0\uB9AC \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\
  \uB294 \uAC83\uC740 \uB9CE\uC740 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\uC5D0\
  \uC11C \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC73C\uB85C, \uB514\uB809\uD1A0\uB9AC\
  \ \uAD6C\uC870\uC758 \uC874\uC7AC \uC5EC\uBD80\uC5D0 \uB530\uB77C \uC870\uAC74\uBD80\
  \ \uC791\uC5C5\uC744 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD30C\uC77C \uC870\
  \uC791, \uC790\uB3D9 \uC2A4\uD06C\uB9BD\uD2B8, \uADF8\uB9AC\uACE0 \uC18C\uD504\uD2B8\
  \uC6E8\uC5B4\uC758 \uCD08\uAE30 \uC124\uC815 \uACFC\uC815\uC5D0\uC11C \uD544\uC694\
  \uD55C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC7A5\uC18C\uC5D0 \uC788\uB294\uC9C0 \uD655\
  \uC778\uD558\uAC70\uB098 \uB514\uB809\uD1A0\uB9AC\uB97C \uC911\uBCF5\uC73C\uB85C\
  \u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디렉토리 존재 여부를 확인하는 것은 많은 프로그래밍 작업에서 기본적인 작업으로, 디렉토리 구조의 존재 여부에 따라 조건부 작업을 가능하게 합니다. 파일 조작, 자동 스크립트, 그리고 소프트웨어의 초기 설정 과정에서 필요한 디렉토리가 장소에 있는지 확인하거나 디렉토리를 중복으로 생성하지 않도록 하기 위해 중요합니다.

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
