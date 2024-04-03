---
date: 2024-01-26 04:15:47.711388-07:00
description: "\uBC29\uBC95: Node.js\uC5D0\uB294 \uD130\uBBF8\uB110\uC744 \uD1B5\uD574\
  \ \uC811\uADFC\uD560 \uC218 \uC788\uB294 REPL\uC774 \uD3EC\uD568\uB418\uC5B4 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC5F4\uAE30\uB9CC \uD558\uBA74 \uC0AC\uC6A9 \uC900\uBE44\uAC00\
  \ \uB05D\uB0A9\uB2C8\uB2E4. \uB9DB\uBCF4\uAE30\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.792594-06:00'
model: gpt-4-0125-preview
summary: "Node.js\uC5D0\uB294 \uD130\uBBF8\uB110\uC744 \uD1B5\uD574 \uC811\uADFC\uD560\
  \ \uC218 \uC788\uB294 REPL\uC774 \uD3EC\uD568\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
