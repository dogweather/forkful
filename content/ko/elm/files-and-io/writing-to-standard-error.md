---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:18.973205-07:00
description: "\uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uC4F0\uAE30\uB294 \uBA54\uC778\
  \ \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825(\uD45C\uC900 \uCD9C\uB825, stdout)\uACFC\
  \ \uBCC4\uB3C4\uB85C \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uB9AC\
  \uB514\uB809\uC158\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC5D0\uB7EC \uCC98\uB9AC\uC640 \uB85C\uAE45\uC744\
  \ \uB354 \uAD00\uB9AC\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574,\
  \ \uD2B9\uD788 \uB514\uBC84\uAE45\uACFC \uBAA8\uB2C8\uD130\uB9C1\uC744 \uC704\uD574\
  \ \uCD9C\uB825 \uAD6C\uBD84\uC774 \uC911\uC694\uD55C \uD658\uACBD\uC5D0\uC11C \uC774\
  \uB807\uAC8C\u2026"
lastmod: '2024-03-11T00:14:29.052951-06:00'
model: gpt-4-0125-preview
summary: "\uD45C\uC900 \uC5D0\uB7EC(stderr)\uB85C \uC4F0\uAE30\uB294 \uBA54\uC778\
  \ \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825(\uD45C\uC900 \uCD9C\uB825, stdout)\uACFC\
  \ \uBCC4\uB3C4\uB85C \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uB9AC\
  \uB514\uB809\uC158\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC5D0\uB7EC \uCC98\uB9AC\uC640 \uB85C\uAE45\uC744\
  \ \uB354 \uAD00\uB9AC\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574,\
  \ \uD2B9\uD788 \uB514\uBC84\uAE45\uACFC \uBAA8\uB2C8\uD130\uB9C1\uC744 \uC704\uD574\
  \ \uCD9C\uB825 \uAD6C\uBD84\uC774 \uC911\uC694\uD55C \uD658\uACBD\uC5D0\uC11C \uC774\
  \uB807\uAC8C\u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

표준 에러(stderr)로 쓰기는 메인 프로그램 출력(표준 출력, stdout)과 별도로 에러 메시지와 진단을 리디렉션하는 것을 말합니다. 프로그래머들은 에러 처리와 로깅을 더 관리하기 쉽게 만들기 위해, 특히 디버깅과 모니터링을 위해 출력 구분이 중요한 환경에서 이렇게 합니다.

## 어떻게:

Elm은 주로 웹 개발을 대상으로 하며, 여기서 stderr로 직접 쓰기 개념은 전통적인 커맨드 라인 환경에서와 같이 적용되지 않습니다. 그러나 Node.js 또는 유사한 환경에서 실행되는 Elm 프로그램의 경우, 포트를 사용한 JavaScript와의 인터옵(interop)이 비슷한 기능을 달성하기 위한 핵심 접근 방식입니다. 여기에 설정 방법을 설명합니다:

Elm 코드 (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- JS로 에러 메시지를 보내는 예제 함수
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "이것은 stderr에 대한 에러 메시지입니다"
```

JavaScript 인터옵 (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

이 Elm 코드는 Elm에서 JavaScript로 메시지를 보낼 수 있게 해주는 포트 `errorOut`를 정의합니다. 그런 다음 JavaScript 코드에서 이 포트를 통해 전송된 메시지를 수신하고 `console.error()`를 사용하여 stderr로 리디렉션합니다. 이 방법을 통해, Elm의 JavaScript와의 인터옵 기능을 활용하여 지원 환경에서 효과적으로 stderr로 쓸 수 있습니다.

Node.js 터미널에서의 샘플 출력 (`index.js` 실행시):
```
이것은 stderr에 대한 에러 메시지입니다
```
