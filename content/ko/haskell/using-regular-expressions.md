---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜 사용하는가?)
정규 표현식은 문자열의 패턴을 찾고 처리하는 데 사용됩니다. 프로그래머는 반복적인 문자열 작업을 자동화하고, 데이터를 검증하며, 복잡한 텍스트 처리를 간단하게 하기 위해 사용합니다.

## How to: (사용법)
Haskell에서 정규 표현식을 사용하기 위해 `regex-posix` 라이브러리를 사용할 수 있습니다. 아래 예제를 통해 기본적인 사용법을 알아보겠습니다.

```Haskell
-- 필수 모듈 임포트
import Text.Regex.Posix

-- 문자열이 패턴과 매칭되는지 확인하는 함수
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern

main :: IO ()
main = do
    let text = "Haskell 재밌다!"
    let pattern = "재밌다"
    print $ isMatch text pattern  -- 결과: True

    -- 정규 표현식을 사용한 문자열 대체
    let replaced = subRegex (mkRegex "재밌다") text "좋다"
    putStrLn replaced  -- 결과: "Haskell 좋다!"
```

## Deep Dive (심층 탐구)
정규 표현식은 1950년대 수학자 스티븐 클리니에 의해 개발되었습니다. Haskell에서는 `regex-posix`, `regex-pcre`, `regex-tdfa` 등 여러 정규 표현식 라이브러리가 있습니다. `regex-posix`는 POSIX 호환 정규 표현식을 지원하며, `regex-pcre`는 Perl 호환, `regex-tdfa`는 좀 더 포괄적인 표현을 지원합니다. 이들은 내부적으로 DFAs(Deterministic Finite Automata) 또는 NFAs(Non-deterministic Finite Automata)로 구현되어 있어서 각각의 성능과 특징 차이를 가집니다.

## See Also (참고 자료)
- Haskell 정규 표현식 패키지: [Hackage regex-posix](https://hackage.haskell.org/package/regex-posix)
- 정규 표현식에 대한 깊은 이해: [Regular Expressions info](https://www.regular-expressions.info/)
- Haskell 공식 문서: [Haskell Documentation](https://www.haskell.org/documentation/)
