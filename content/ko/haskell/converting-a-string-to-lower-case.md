---
title:                "Haskell: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 소문자로 변환하는 것의 장점은 무엇일까요? 이 기능은 한국어를 비롯한 다른 언어에서 대소문자를 고려해야 할 때 특히 유용합니다. 또한 문자열을 처리하고 분석할 때 일관성을 유지하는 데 도움이 됩니다.

## 어떻게?

Haskell에서 문자열을 소문자로 변환하는 방법은 매우 쉽습니다. ```toLower``` 함수를 사용하면 됩니다. 아래의 예제 코드를 참조하세요.

```Haskell
import Data.Char

toLower "THIS IS A SAMPLE STRING." -- 결과: "this is a sample string."
```

또는, 문자열 대신 문자를 변환하고 싶다면 다음과 같이 할 수 있습니다.

```Haskell
toLower 'A' -- 결과: 'a'
```

위의 예시에서 보듯이 ```toLower``` 함수는 모든 알파벳을 소문자로 바꾸는 것 외에도, 숫자나 특수 문자는 그대로 둡니다.

## 깊게 들어가보기

그렇다면 왜 Haskell의 ```toLower``` 함수는 문자열을 소문자로 변환할 때만 작동할까요? 이는 Haskell의 강한 타입 시스템 때문입니다. 즉, 각 데이터 유형마다 엄격한 규칙이 적용되기 때문입니다. 이 규칙 때문에 특정한 타입의 값이 필요한 함수에만 적용됩니다. 예를 들어, ```toLower``` 함수는 문자열에 대해서만 작동하도록 정확히 타입이 정의되어 있습니다. 따라서 우리는 문자열을 전달하지 않으면 함수를 사용할 수 없습니다.

그러나 이것은 일종의 보호장치일 뿐만 아니라, 디버깅할 때도 유용합니다. 만약 우리가 잘못된 타입의 값을 전달하려고 하면 Haskell은 이를 런타임에 차단하여 프로그램이 실행되지 않도록 합니다. 이렇게 함으로써 우리는 더 많은 시간을 코드의 질을 향상시키는 데 사용할 수 있습니다.

## 참고

[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - Haskell을 배우는 데 유용한 무료 온라인 책입니다.
[Haskell Language](https://www.haskell.org/) - Haskell 공식 사이트입니다. 다양한 정보와 자료를 제공합니다.
[Real World Haskell](http://book.realworldhaskell.org/) - 실제로 Haskell을 사용하는 방법에 대한 지침서입니다.