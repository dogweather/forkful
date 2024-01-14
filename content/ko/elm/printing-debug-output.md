---
title:                "Elm: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 하려는 이유는 무엇일까요? 디버그 출력은 프로그래밍 과정에서 발생하는 오류를 찾고 수정할 때 유용합니다.

## 이렇게 해보세요

```Elm
-- 디버그 출력을 활성화 합니다.
import Debug exposing (log)

-- 변수에 디버그 메시지를 할당합니다.
foo : Int
foo = 10 |> log "Foo 변수 값"

-- 출력 결과는 다음과 같습니다.
Foo 변수 값: 10 : Int

```

디버그 출력을 활용하여 변수나 함수의 값이 어떻게 변하는지 쉽게 추적할 수 있습니다. 이를 통해 오류를 빠르게 발견하고 수정할 수 있습니다.

## 깊이 들어가기

디버그 출력은 일반적으로 개발할 때 사용되는 기법입니다. 그러나 실제로 릴리스를 할 때는 출력이 발생하지 않도록 최종 코드에서 디버그 모듈을 제거하는 것이 좋습니다. Elm 컴파일러는 `--optimize` 플래그를 사용하여 모든 디버그 코드를 삭제할 수 있도록 해주기 때문입니다. 또한 디버그 출력을 위한 다양한 함수들이 존재합니다. `log` 외에도 `log2`, `log3` 등의 함수를 사용하여 튜플이나 리스트 형태의 값을 출력할 수 있습니다.

## 관련 정보

"See Also" 

- Elm 디버깅 문서: https://guide.elm-lang.org/debugging/
- Elm 디버깅과 관련된 명령어: https://package.elm-lang.org/packages/elm/core/latest/Debug