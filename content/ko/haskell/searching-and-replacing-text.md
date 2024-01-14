---
title:                "Haskell: 텍스트 검색 및 교체"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트 검색 및 대체를 하려는 이유에 대해 간단히 설명합니다.

텍스트 검색과 대체는 많은 프로그래밍 작업에서 필수적인 단계입니다. 예를 들어, 파일 내의 특정 텍스트를 찾고 해당 텍스트를 대체하는 작업은 큰 규모의 데이터 처리 프로젝트에서 매우 중요합니다.

## 어떻게

Haskell에서 검색 및 대체 작업을 하는 방법에 대한 코딩 예제와 샘플 출력을 ```Haskell ...``` 코드 블록 내에서 제공합니다.

예를 들어, 우리가 "apple"이라는 단어를 "banana"로 바꾸는 코드를 생각해봅시다.

```Haskell
-- 텍스트를 읽어옴
text <- readFile "input.txt"

-- "apple"을 "banana"로 대체
let replacedText = replace "apple" "banana" text

-- 결과 출력
putStrLn replacedText
```

이 코드를 실행하면 "input.txt" 파일 내의 모든 "apple"이 "banana"로 대체된 새로운 파일이 생성됩니다.

## 심층 분석

검색과 대체는 일반적으로 정규 표현식(regular expressions)을 사용하여 수행됩니다. 이는 주어진 패턴과 일치하는 모든 텍스트를 찾아내는 유용한 방법입니다. Haskell에서는 "regex-tdfa"와 같은 패키지를 사용하여 간단하게 정규 표현식을 적용할 수 있습니다.

또한, 입력 및 출력(입출력)을 다루기 위해 IO 모나드를 사용해야 하는 것에 주의해야 합니다. 이는 순수 함수형 프로그래밍 언어로서 Haskell의 중요한 특징 중 하나입니다.

## 참고 자료

- [Haskell regex-tdfa 패키지 문서](https://hackage.haskell.org/package/regex-tdfa)
- [Haskell IO 모나드 자세히 보기](https://en.wikibooks.org/wiki/Haskell/Understanding_the_IO_monad)