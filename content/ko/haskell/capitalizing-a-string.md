---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

category:             "Haskell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 대문자화하는 것은, 모든 문자를 대문자로 변환하는 과정입니다. 프로그래머들은 명확성과 형식을 통일하기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
Haskell에서 문자열의 각 문자를 대문자로 만드는 가장 간단한 방법은 `Data.Char` 모듈의 `toUpper` 함수를 사용하는 것입니다.

```haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

main :: IO ()
main = putStrLn $ capitalize "haskell 코드를 대문자로!"
```

샘플 출력:
```
HASKELL 코드를 대문자로!
```

## Deep Dive (심층 분석)
대문자화는 컴퓨터 과학에서 문자열을 정규화하는 기본적인 방법 중 하나입니다. 유닉스 시스템에서 파일 이름과 같이 대소문자를 구분하는 경우에도, 특정 조건 하에 정보를 동일 시하는데 사용됩니다.

`Data.Char`는 여러 문자 관련 유틸리티를 제공하며, `toUpper`는 그 중 하나입니다. 이 함수는 아스키 코드뿐만 아니라 유니코드 문자에 대해서도 대문자 변환을 지원합니다.

`map toUpper` 함수를 사용하는 것 외에도 `Text` 라이브러리를 사용해 대문자화를 수행할 수 있으며, 이는 대규모 텍스트 처리에 더 효율적입니다.

## See Also (관련 자료)
- `Data.Char` 모듈 문서: http://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- 유니코드 표준: https://unicode.org/standard/standard.html
- Haskell `Text` 라이브러리: http://hackage.haskell.org/package/text
