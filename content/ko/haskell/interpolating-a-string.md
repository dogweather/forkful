---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Haskell에서 문자열 보간하기: 어떻게 할까?

## 무엇 & 왜?
문자열 보간(string interpolation)이란, 문자열 안에 변수나 표현식을 삽입하여 새로운 문자열을 생성하는 것입니다. 이는 문자열을 조작하고 출력을 자동화하는데 있어 편리하고 유용한 스킬입니다.

## 어떻게:
```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Str

main :: IO ()
main = do
    let name = "John Doe"
    let age = 23
    print $ str "이름: ${name}, 나이: ${age}"
```
이 코드를 실행하면, `"이름: John Doe, 나이: 23"`라는 문자열이 출력됩니다.

## 깊게 알아보기
- 하스켈에서 문자열 보간을 가장 일반적으로 사용하는 방법은 "str" 데이터 타입을 사용하는 것입니다. 이는 문자열 보간을 Haskell에 빌트인 PLC 문자열에 대한 언어 확장으로 추가하였습니다.
- 하스켈의 다른 문자열 보간 방법으로는 "Text.Printf" 안의 프린트프 처리를 활용하는 법이 있습니다. 이 방법이 더 강력할 수 있지만, 상황에 따라 코드를 복잡하게 만드는 단점이 있습니다.
- 문자열 보간 구현의 주된 이슈 중 하나는 언어 확장의 사용에 따른 포터빌리티와 생산성 사이의 트레이드오프입니다. 일부 프로그래머들은 표준화된 텍스트 가공 라이브러리를 사용하느니 보다 네이티브하게 문자열을 처리하는 것을 선호합니다.

## 참고 출처들
- [Haskell 문자열 보간의 공식 문서](https://hackage.haskell.org/package/interpolate)
- [Haskell에 대한 문자열 처리 가이드](https://wiki.haskell.org/String_interpolation)
- ['printf' 처리에 대한 여러 예제 코드들](http://book.realworldhaskell.org/read/profiling-and-optimization.html)