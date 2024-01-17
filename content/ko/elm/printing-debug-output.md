---
title:                "디버그 출력 프린트하기"
html_title:           "Elm: 디버그 출력 프린트하기"
simple_title:         "디버그 출력 프린트하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래머들이 디버그 출력을 하는 이유에 대해 알아보기 전에 우선 디버그 출력이 무엇인지 알아보겠습니다. 디버그 출력은 일반적으로 코드 실행 중에 프로그램의 상태를 확인하기 위해 사용되는 코드입니다. 사용자가 원하는 결과를 출력하는지, 어떤 값이 할당되었는지 등을 확인할 수 있습니다. 이렇게 상태를 확인하고 디버그하는 것은 프로그래밍 과정에서 매우 중요합니다.

## 방법:

Elm에서 디버그 출력을 하는 방법은 매우 간단합니다. 아래 예시를 따라해보세요.

```Elm
-- 문자열 출력
Debug.log "문자열" "안녕하세요!"

-- 숫자 출력
Debug.log "숫자" 123

-- 리스트 출력
Debug.log "리스트" [1, 2, 3]
```

출력 결과는 다음과 같이 터미널에 표시됩니다.

```
문자열: "안녕하세요!"
숫자: 123
리스트: [1, 2, 3]
```

## 깊이 살펴보기:

디버그를 위해 다른 언어에서는 프린트 문을 사용하곤 했습니다. 하지만 Elm에서는 리스트나 튜플 등과 같은 복잡한 데이터를 자동으로 표시 가능하도록 최적화되어 있습니다. 또한, 디버그 출력을 사용하는 대신에 디버거를 활용할 수도 있습니다. Elm의 디버거는 더 많은 정보를 제공하며 디버깅 과정을 보다 쉽게 만들어줍니다. 또한, Elm's console 라이브러리를 사용하면 웹 브라우저의 개발자 도구 콘솔에서 디버그 출력을 확인할 수 있습니다.

## 더 보기:

- [Elm 디버깅 가이드](https://guide.elm-lang.org/debugging/)
- [Elm 디버거](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Debugger)
- [Elm's console 라이브러리](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Console)