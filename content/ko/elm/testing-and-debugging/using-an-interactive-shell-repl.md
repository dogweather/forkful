---
date: 2024-01-26 04:13:53.587428-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Elm\uC740 \uD1B5\uD569 REPL\uC744 \uAC16\uCD94\
  \uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098 Elm\uC744 \uC124\
  \uCE58\uD55C \uD6C4 \uCEE4\uB9E8\uB4DC \uB77C\uC778\uC5D0\uC11C `elm repl`\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC Elm \uC138\uC158\uC744 \uC2DC\uC791\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.112764-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uD1B5\uD569 REPL\uC744 \uAC16\uCD94\uACE0 \uC788\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
