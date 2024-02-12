---
title:                "인터랙티브 셸 (REPL) 사용하기"
aliases: - /ko/elm/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:53.587428-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
Read-Eval-Print Loop (REPL)은 단일 사용자 입력을 받아 평가하고 결과를 사용자에게 반환하는 간단한 상호작용형 프로그래밍 환경입니다. Elm 프로그래머들은 REPL을 사용하여 빠른 실험, 디버깅 또는 언어 학습을 합니다.

## 사용 방법:
Elm은 통합 REPL을 갖추고 있지 않습니다. 그러나 Elm을 설치한 후 커맨드 라인에서 `elm repl`을 사용하여 Elm 세션을 시작할 수 있습니다.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

이 세션에서는 List 함수들을 가져온 후 리스트 안의 숫자들을 두 배로 증가시켰고, 즉시 결과를 얻었습니다.

## 심층 탐구
Elm의 REPL은 Python이나 JavaScript와 같은 일부 다른 언어의 REPL과 비교하여 제한적으로 보일 수 있습니다. 왜냐하면 Elm은 웹 앱 생성에 중점을 둔 컴파일 언어이기 때문입니다. 역사적으로 Elm은 스크립팅이나 쉘 상호작용보다는 전체 어플리케이션에 중점을 두었습니다.

Elm의 REPL에 대한 대안으로는 `elm-live` 및 브라우저에서 실시간으로 코드 변경 사항을 볼 수 있는 온라인 편집기인 Ellie가 있습니다.

구현과 관련하여 Elm REPL은 백그라운드에서 Elm 코드 조각을 JavaScript로 컴파일하여 Elm을 상호작용식으로 실행할 수 있게 합니다. 이는 해석 언어의 REPL과 다르며, 컴파일 단계가 필요하지 않습니다. Elm REPL은 핵심 언어를 가볍고 중점적으로 유지하기 위해 간소화되어 있습니다.

## 참고 자료
- Elm의 공식 상호작용 가이드: https://guide.elm-lang.org/interop/
- Ellie, 온라인 Elm 놀이터: https://ellie-app.com/new
- `elm-live`, Elm용 유연한 개발 서버: https://www.elm-live.com/
