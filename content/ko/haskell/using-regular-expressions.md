---
title:                "정규 표현식 사용하기"
date:                  2024-02-03T19:17:08.206749-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
