---
date: 2024-01-26 04:15:47.711388-07:00
description: "\uB300\uD654\uD615 \uC178 \uB610\uB294 REPL(Read-Eval-Print Loop, \uC77D\
  \uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504)\uC740 \uC989\uC11D\uC5D0\uC11C \uCF54\
  \uB4DC\uB97C \uC2E4\uD589\uD558\uC5EC \uD568\uC218, \uC54C\uACE0\uB9AC\uC998\uC744\
  \ \uD14C\uC2A4\uD2B8\uD558\uAC70\uB098 \uC544\uC774\uB514\uC5B4\uB97C \uC2DC\uB3C4\
  \uD574 \uBCFC \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uCF54\uB529\uC758 \uBA54\
  \uBAA8\uC7A5 \uAC19\uC740 \uAC83\uC73C\uB85C, \uC804\uCCB4 \uAC1C\uBC1C \uD658\uACBD\
  \uC744 \uC124\uC815\uD558\uC9C0 \uC54A\uACE0\uB3C4 \uBE60\uB974\uACE0 \uAC04\uD3B8\
  \uD558\uAC8C \uC0AC\uC6A9\uD560 \uC218\u2026"
lastmod: '2024-02-25T18:49:52.781587-07:00'
model: gpt-4-0125-preview
summary: "\uB300\uD654\uD615 \uC178 \uB610\uB294 REPL(Read-Eval-Print Loop, \uC77D\
  \uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504)\uC740 \uC989\uC11D\uC5D0\uC11C \uCF54\
  \uB4DC\uB97C \uC2E4\uD589\uD558\uC5EC \uD568\uC218, \uC54C\uACE0\uB9AC\uC998\uC744\
  \ \uD14C\uC2A4\uD2B8\uD558\uAC70\uB098 \uC544\uC774\uB514\uC5B4\uB97C \uC2DC\uB3C4\
  \uD574 \uBCFC \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uCF54\uB529\uC758 \uBA54\
  \uBAA8\uC7A5 \uAC19\uC740 \uAC83\uC73C\uB85C, \uC804\uCCB4 \uAC1C\uBC1C \uD658\uACBD\
  \uC744 \uC124\uC815\uD558\uC9C0 \uC54A\uACE0\uB3C4 \uBE60\uB974\uACE0 \uAC04\uD3B8\
  \uD558\uAC8C \uC0AC\uC6A9\uD560 \uC218\u2026"
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
대화형 셸 또는 REPL(Read-Eval-Print Loop, 읽기-평가-출력 루프)은 즉석에서 코드를 실행하여 함수, 알고리즘을 테스트하거나 아이디어를 시도해 볼 수 있게 해줍니다. 코딩의 메모장 같은 것으로, 전체 개발 환경을 설정하지 않고도 빠르고 간편하게 사용할 수 있습니다.

## 방법:
Node.js에는 터미널을 통해 접근할 수 있는 REPL이 포함되어 있습니다. 열기만 하면 사용 준비가 끝납니다. 맛보기입니다:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

간단하죠? 변수를 정의하고, 함수를 실행하거나 루프를 돌리세요. 완료되면 `.exit`이 실제 세계로 돌아가게 합니다.

## 심층 탐구
REPL은 1960년대부터 있었습니다 – LISP가 이 개념을 선도했습니다. 아이디어는 프로그래머에게 즉각적인 피드백을 제공하는 것입니다. 대안은? Node.js REPL 외에도 Chrome DevTools 같은 브라우저 기반 콘솔, JSFiddle과 같은 온라인 샌드박스, VSCode와 같은 대화형 플레이그라운드가 있는 전체 IDE 등이 있습니다.

내부적으로 REPL 워크플로우는 일반적으로:
1. 입력 읽기
2. 코드 컴파일 및 실행
3. 출력 출력
4. 다시 루프로

단순하지만 대화식 코딩에 막대한 영향을 미친 효과적인 사이클입니다.

## 또한 보기
- [Node.js REPL 문서](https://nodejs.org/api/repl.html)
- [Mozilla의 REPL에서의 JavaScript 모듈 소개](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
