---
date: 2024-01-20 17:50:50.797940-07:00
description: "How to: (\uBC29\uBC95) Elm\uC758 \uCD5C\uC2E0 \uBC84\uC804\uC5D0\uC11C\
  \uB294 \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774 \uC9C1\uC811\uC801\uC73C\uB85C \uC9C0\
  \uC6D0\uB418\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uB300\uC2E0, `String` \uBAA8\uB4C8\
  \uC758 \uD568\uC218\uB97C \uC0AC\uC6A9\uD574\uC57C \uD569\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uADF8 \uC608\uC2DC\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.849032-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Elm\uC758 \uCD5C\uC2E0 \uBC84\uC804\uC5D0\uC11C\uB294 \uBB38\
  \uC790\uC5F4 \uBCF4\uAC04\uC774 \uC9C1\uC811\uC801\uC73C\uB85C \uC9C0\uC6D0\uB418\
  \uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (방법)
Elm의 최신 버전에서는 문자열 보간이 직접적으로 지원되지 않습니다. 대신, `String` 모듈의 함수를 사용해야 합니다. 다음은 그 예시입니다:

```Elm
name = "Jane"
greeting = "Hello, " ++ name ++ "!"

-- 출력: "Hello, Jane!"
```

만약 여러 값을 하나의 문자열로 결합하려면, `String.concat` 혹은 `++` 연산자를 사용하세요:

```Elm
age = 25
welcomeMessage = "Welcome, " ++ name ++ "! You are " ++ String.fromInt(age) ++ " years old."

-- 출력: "Welcome, Jane! You are 25 years old."
```

## Deep Dive (심층 탐구)
Elm에서는 다른 언어들처럼 백틱(`)이나 특수한 문자열 구문(`${}`)을 사용한 문자열 보간이 없습니다. 대신, 문자열을 결합하기 위해 `++` 연산자를 사용하거나 여러 값을 가진 리스트를 `String.concat`으로 합치는 방법을 사용해야 합니다. 이는 Elm의 단순함과 명료성에 기반한 디자인 선택입니다.

역사적으로, Elm은 JavaScipt와의 호환성에 집중했지만, 문자열 보간과 관련해 직접적인 JS의 템플릿 리터럴 대응물을 갖고 있지 않습니다. 문자열 연산의 경우, Elm의 타입 안전성과 오류를 줄이기 위한 목적에서 명시적으로 값들을 문자열로 변환하고 합치는 것을 선택했습니다.

## See Also (참고 자료)
- Elm 문서: [String 모듈](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm 문서: [연산자](https://package.elm-lang.org/packages/elm-lang/core/latest/Basics#operators)
- Elm 커뮤니티 예시와 토론: [Elm Discourse](https://discourse.elm-lang.org/)
