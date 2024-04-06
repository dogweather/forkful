---
date: 2024-01-20 17:42:42.592054-07:00
description: "\uC2E4\uD589 \uBC29\uBC95: - **\uC5ED\uC0AC\uC801 \uB9E5\uB77D**: \uBB38\
  \uC790\uC5F4 \uCC98\uB9AC\uB294 \uCEF4\uD4E8\uD130 \uD504\uB85C\uADF8\uB798\uBC0D\
  \uC758 \uADFC\uAC04 \uC911 \uD558\uB098\uB85C, \uCD08\uAE30 \uD504\uB85C\uADF8\uB798\
  \uBC0D \uC5B8\uC5B4\uBD80\uD130 \uD604\uB300 \uC5B8\uC5B4\uC5D0 \uC774\uB974\uAE30\
  \uAE4C\uC9C0 \uBC1C\uC804\uD574\uC654\uC2B5\uB2C8\uB2E4. Haskell\uACFC \uAC19\uC740\
  \ \uD568\uC218\uD615 \uC5B8\uC5B4\uB294 \uC774\uB7EC\uD55C \uBB38\uC790\uC5F4 \uCC98\
  \uB9AC \uC791\uC5C5\uC744 \uBD88\uBCC0\uC131\uACFC \uD568\uC218 \uC870\uD569\uC73C\
  \uB85C \uCC98\uB9AC\uD569\uB2C8\uB2E4. - **\uB300\uC548**:\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.991421-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
