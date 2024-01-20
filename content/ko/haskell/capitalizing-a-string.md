---
title:                "문자열 대문자화"
html_title:           "Haskell: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 대문자화는 모든 알파벳 소문자를 대문자로 바꾸는 것입니다. 이 작업은 사용자 입력 준비나 가독성 향상을 위해 프로그래머들이 종종 실행합니다.

## 어떻게:
하스켈에서 문자열을 대문자로 만드는 간단한 예를 보겠습니다:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper
```

사용 예시:

```Haskell
main = print(capitalize "hello world")
```

출력:

```Haskell
"HELLO WORLD"
```

## 깊게 알아보기
**역사적 맥락**: 문자열의 대문자화는 컴퓨터 프로그래밍의 초기부터 존재했습니다. 일반적으로 문자열 검색, 비교, 정렬 등의 작업을 수행하기 위해 사용됩니다.

**대안**: 위의 코드는 `toUpper` 함수를 사용하여 각 문자를 대문자로 변경합니다. 연속적인 문자 목록을 대상으로 `toUpper` 함수를 적용하려면 `map` 고차함수를 사용합니다. 다른 대안으로는 `Data.Text` 라이브러리의 `toUpper` 함수를 사용할 수 있습니다.

**구현 세부 정보**: `Data.Char` 라이브러리의 `toUpper` 함수는 유니코드 스칼라 값을 대문자로 변환합니다. 이 함수는 특정 언어의 대소문자 변환 규칙을 따르지 않습니다. 특수 대소문자 규칙을 따르려면 `Data.Text.ICU` 라이브러리를 사용해야 할 수 있습니다.

## 함께 보기
아래 링크에서 더 많은 정보와 예제를 찾을 수 있습니다:

- 문자열 대문자화에 대한 자세한 정보: [Haskell Documentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html#v:toUpper)
- Data.Char 라이브러리에 대한 자세한 정보: [Haskell Data.Char Library](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- Data.Text 라이브러리에 대한 자세한 정보: [Haskell Data.Text Library](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)
- 특수 대소문자 규칙을 따르는 문자열 대문자화: [Haskell Data.Text.ICU Library](https://hackage.haskell.org/package/text-icu-0.7.0.1/docs/Data-Text-ICU.html)