---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:42:42.592054-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
패턴과 일치하는 문자 삭제는 특정 기준에 맞는 문자들을 문자열에서 제거하는 것을 의미합니다. 이 작업은 데이터를 정제하거나, 불필요한 부분을 제거하여 프로그램의 입력을 깔끔하게 다듬기 위해 수행됩니다.

## 실행 방법:
```Haskell
import Data.List (intercalate)
import Text.Regex.Posix ((=~))

-- 패턴에 맞는 문자 삭제
deletePattern :: String -> String -> String
deletePattern pat str = intercalate "" (str =~ pat :: [String])

main :: IO ()
main = do
    let str = "안녕하세요, Haskell!"
    let pattern = "[^가-힣]" -- 한글이 아닌 모든 문자에 대한 패턴
    let result = deletePattern pattern str
    putStrLn result -- "안녕하세요"
```

## 깊은 이해:
- **역사적 맥락**: 문자열 처리는 컴퓨터 프로그래밍의 근간 중 하나로, 초기 프로그래밍 언어부터 현대 언어에 이르기까지 발전해왔습니다. Haskell과 같은 함수형 언어는 이러한 문자열 처리 작업을 불변성과 함수 조합으로 처리합니다.
- **대안**: `Data.Text` 모듈을 사용하는 것도 한 방법입니다. `Text` 타입은 문자열을 더 효율적으로 다룹니다. 또한, 직접 정규 표현식 대신 `filter` 함수와 같은 리스트 고차 함수를 사용해서 특정 조건을 만족하는 문자만 유지하는 방법도 있습니다.
- **구현 세부사항**: 패턴 매칭은 정규 표현식을 기반으로 수행됩니다. Haskell에서는 `Text.Regex.Posix` 모듈을 이용해 POSIX 호환 정규 표현식을 적용할 수 있습니다. 이 예제에서는 `=~` 연산자를 사용해 문자열을 패턴에 매칭되지 않는 부분들로 나누고, 이를 다시 결합하여 목표 문자열을 얻었습니다.

## 참고자료:
- [Hoogle](https://hoogle.haskell.org/): Haskell 표준 라이브러리 검색 엔진.
- [Haskell Language](https://www.haskell.org/documentation/): Haskell 공식 문서 및 자료.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/): Haskell 입문서.
- [Regular Expressions](https://wiki.haskell.org/Regular_expressions): Haskell Wiki에서 정규 표현식에 대한 논의.
