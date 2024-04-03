---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:08.206749-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Haskell\uC5D0\uC11C\uB294 \uC815\uADDC \uD45C\
  \uD604\uC2DD \uAE30\uB2A5\uC774 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758\
  \ \uC77C\uBD80\uAC00 \uC544\uB2C8\uAE30 \uB54C\uBB38\uC5D0, `regex-base`\uC640 \uAC19\
  \uC740 \uC81C3\uC790 \uD328\uD0A4\uC9C0\uC640 POSIX \uC815\uADDC \uD45C\uD604\uC2DD\
  \ \uC9C0\uC6D0\uC744 \uC704\uD55C `regex-posix`, Perl \uD638\uD658 \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uC704\uD55C `regex-pcre` \uB4F1\uACFC \uAC19\uC740 \uD638\
  \uD658\u2026"
lastmod: '2024-03-13T22:44:55.281411-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C\uB294 \uC815\uADDC \uD45C\uD604\uC2DD \uAE30\uB2A5\uC774\
  \ \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uC77C\uBD80\uAC00 \uC544\uB2C8\
  \uAE30 \uB54C\uBB38\uC5D0, `regex-base`\uC640 \uAC19\uC740 \uC81C3\uC790 \uD328\uD0A4\
  \uC9C0\uC640 POSIX \uC815\uADDC \uD45C\uD604\uC2DD \uC9C0\uC6D0\uC744 \uC704\uD55C\
  \ `regex-posix`, Perl \uD638\uD658 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC704\uD55C\
  \ `regex-pcre` \uB4F1\uACFC \uAC19\uC740 \uD638\uD658 \uBC31\uC5D4\uB4DC\uC758 \uC0AC\
  \uC6A9\uC774 \uD544\uC694\uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

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
