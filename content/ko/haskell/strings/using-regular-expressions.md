---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:08.206749-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\
  \uC2DD\uC740 \uAC80\uC0C9 \uD328\uD134\uC744 \uC815\uC758\uD558\uB294 \uBB38\uC790\
  \uC758 \uC5F0\uC18D\uC73C\uB85C, \uC77C\uBC18\uC801\uC73C\uB85C \uBB38\uC790\uC5F4\
  \ \uAC80\uC0C9 \uBC0F \uC870\uC791\uC5D0 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. Haskell\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB2E8\uC21C\uD55C \uBB38\uC790\uC5F4\
  \ \uC77C\uCE58\uBD80\uD130 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\uC5D0\
  \ \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC791\uC5C5\uC5D0 \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uD65C\uC6A9\uD558\uC5EC, \uD14D\uC2A4\uD2B8 \uB370\uC774\
  \uD130\uB97C \uB2E4\uB8E8\uB294 \uB370 \uC788\uC5B4 \uADF8\u2026"
lastmod: '2024-03-11T00:14:29.201329-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\
  \uC740 \uAC80\uC0C9 \uD328\uD134\uC744 \uC815\uC758\uD558\uB294 \uBB38\uC790\uC758\
  \ \uC5F0\uC18D\uC73C\uB85C, \uC77C\uBC18\uC801\uC73C\uB85C \uBB38\uC790\uC5F4 \uAC80\
  \uC0C9 \uBC0F \uC870\uC791\uC5D0 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. Haskell \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB2E8\uC21C\uD55C \uBB38\uC790\uC5F4 \uC77C\uCE58\
  \uBD80\uD130 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\uC5D0 \uC774\uB974\
  \uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC791\uC5C5\uC5D0 \uC815\uADDC \uD45C\uD604\
  \uC2DD\uC744 \uD65C\uC6A9\uD558\uC5EC, \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C\
  \ \uB2E4\uB8E8\uB294 \uB370 \uC788\uC5B4 \uADF8\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
프로그래밍에서 정규 표현식은 검색 패턴을 정의하는 문자의 연속으로, 일반적으로 문자열 검색 및 조작에 사용됩니다. Haskell 프로그래머들은 단순한 문자열 일치부터 복잡한 텍스트 처리에 이르기까지 다양한 작업에 정규 표현식을 활용하여, 텍스트 데이터를 다루는 데 있어 그 효율성과 다양성을 최대한 활용합니다.

## 사용 방법:
Haskell에서는 정규 표현식 기능이 표준 라이브러리의 일부가 아니기 때문에, `regex-base`와 같은 제3자 패키지와 POSIX 정규 표현식 지원을 위한 `regex-posix`, Perl 호환 정규 표현식을 위한 `regex-pcre` 등과 같은 호환 백엔드의 사용이 필요합니다. 다음은 이러한 패키지를 사용하여 정규 표현식으로 작업하는 방법입니다.

먼저, `.cabal` 파일에 `regex-posix`나 `regex-pcre`를 추가하거나 직접 cabal을 통해 패키지가 설치되어 있는지 확인하세요:

```bash
cabal install regex-posix
```
또는
```bash
cabal install regex-pcre
```

### `regex-posix` 사용하기:

```haskell
import Text.Regex.Posix ((=~))

-- 문자열이 패턴과 일치하는지 확인
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- 첫 번째 일치 항목 찾기
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- 출력: True
    print $ findFirst "good morning, good night" "good"
    -- 출력: "good"
```

### `regex-pcre` 사용하기:

```haskell
import Text.Regex.PCRE ((=~))

-- 모든 일치 항목 찾기
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- 출력: ["test1","test2","test3"]
```

각 라이브러리는 특성이 있지만, 일치 항목을 확인하거나 부분 문자열을 추출하는 데 있어 `=~`을 사용하는 일반적인 방법론은 일관적입니다. `regex-posix`와 `regex-pcre` 중에서 선택하는 것은 대부분 프로젝트의 필요와 필요한 특정 정규 표현식 기능에 따라 달라집니다.
