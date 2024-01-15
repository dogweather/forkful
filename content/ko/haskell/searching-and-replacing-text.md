---
title:                "텍스트 찾기 및 대체하기"
html_title:           "Haskell: 텍스트 찾기 및 대체하기"
simple_title:         "텍스트 찾기 및 대체하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 대체하는 것이 중요한 이유는 텍스트에 포함된 정보를 수정하거나 업데이트할 때 유용하기 때문입니다. 특히 프로그래밍에서는 자주 사용되는 일이며, 단순한 작업으로 불필요한 시간과 노력을 절약할 수 있습니다. 

## 어떻게
여러분이 텍스트를 검색하고 대체하는 방법 또한 간단합니다. 헤스켈의 `Text` 라이브러리를 사용하면 간단한 문법으로 작성할 수 있습니다.

```Haskell
import Data.Text

-- "Haskell"을 "파이썬"으로 바꾸는 예제
replaceText :: Text -> Text -> Text -> Text
replaceText old new text = replace old new text

-- "Hello, Haskell!"을 "Hello, 파이썬!"으로 바꾸는 예제
main = do
    let oldText = "Haskell"
    let newText = "파이썬"
    let originalText = "Hello, Haskell!"
    let replacedText = replaceText oldText newText originalText
    print replacedText
```

위의 코드에서는 `Data.Text`를 불러오고, `replaceText` 함수를 정의한 후, `main`에서 새로운 텍스트를 정의한 다음 `replaceText` 함수에 인자로 넘겨준 뒤 새로운 텍스트를 출력하는 간단한 예제를 보여줍니다.

## 딥 다이브
텍스트를 검색하고 대체하는 과정에서 좀 더 심화된 내용은 다음과 같습니다.

- 더 복잡한 검색 규칙을 사용할 수 있습니다. 예를 들어, `replaceAll` 함수를 사용하면 전체 텍스트에서 일치하는 모든 문자열을 바꿀 수 있습니다.
- `Text` 라이브러리 외에도 `String` 라이브러리를 사용할 수 있습니다.
- 특정 파일에서만 텍스트를 검색하고 대체할 수 있도록 하는 방법도 존재합니다.

딥 다이브는 여러분이 자유롭게 실험하고 배우는 과정입니다. 새로운 방법을 발견하고, 여러분만의 유용한 코드를 만들어보세요!

## 참고
- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [Data.Text 문서](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [String vs Text in Haskell](https://stackoverflow.com/questions/105674/what-are-the-differences-between-data-text-text-and-string-in-haskell)