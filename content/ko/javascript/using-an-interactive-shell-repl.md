---
title:                "인터랙티브 셸 (REPL) 사용하기"
date:                  2024-01-26T04:15:47.711388-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-an-interactive-shell-repl.md"
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