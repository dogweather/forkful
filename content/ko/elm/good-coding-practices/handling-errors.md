---
title:                "에러 처리하기"
aliases:
- /ko/elm/handling-errors/
date:                  2024-01-26T00:51:28.026788-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇을, 왜?
오류를 처리한다는 것은 잘못될 수 있는 것들을 예측하고 다룰 수 있는 코드를 작성하는 것을 의미합니다. 프로그래머들은 크래시를 방지하고, 데이터 무결성을 보호하고, 사용자에게 우아한 대체 수단을 제공하기 위해 이를 수행합니다.

## 어떻게 하나요:
Elm의 핵심 철학은 런타임 예외가 없다는 것입니다. 그래서 Elm은 `Maybe`와 `Result`와 같은 타입을 활용하여 오류를 다룹니다.

`Maybe` 시나리오의 경우:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- 실행할 때:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

`Result` 시나리오의 경우:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- 그리고 사용할 때:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## 심층 분석
Elm의 타입 시스템은 엄격하여 조기에 오류를 발견하는 데 도움을 줍니다. 역사적으로 대부분의 언어들은 예외와 런타임 검사에 의존했지만, Elm은 컴파일 시간 보장을 선택했습니다. `Result`와 같은 대안은 자세한 오류 정보를 제공하는 반면, `Maybe`는 예/아니오 시나리오에서 더 단순합니다. Elm의 오류 처리는 개발자들이 모든 경로를 미리 고려하도록 장려하여 잊혀진 오류 케이스의 함정을 피하는 데 도움을 줍니다.

## 또한 보세요:
- 오류 처리에 관한 Elm 공식 가이드 섹션: [오류 처리 – 소개](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` 문서: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` 문서: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
