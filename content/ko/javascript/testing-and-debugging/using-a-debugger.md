---
date: 2024-01-26 03:50:30.098078-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\
  \uC9C0 \uC54A\uB294 JavaScript \uCF54\uB4DC \uC870\uAC01\uC774 \uC5EC\uAE30 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.797075-06:00'
model: gpt-4-0125-preview
summary: "\uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uC9C0 \uC54A\uB294 JavaScript\
  \ \uCF54\uB4DC \uC870\uAC01\uC774 \uC5EC\uAE30 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 사용 방법:
예상대로 동작하지 않는 JavaScript 코드 조각이 여기 있습니다:

```javascript
function buggyMultiply(a, b) {
    return a + b; // 이런! 이것은 덧셈이 아닌 곱셈이어야 합니다.
}

let result = buggyMultiply(5, 3);
console.log('결과:', result);
```

출력이 올바르지 않습니다:
```
결과: 8
```

Chrome DevTools에서 디버깅해 봅시다:

1. 브라우저에서 이 JS를 엽니다.
2. 마우스 오른쪽 버튼을 클릭하고 "검사"를 선택하여 DevTools를 엽니다.
3. "Sources" 탭을 클릭합니다.
4. 코드 스니펫이나 페이지를 찾고 `return` 문 옆에 있는 줄 번호를 클릭하여 중단점을 설정합니다.
5. 페이지를 새로 고침하여 중단점을 발생시킵니다.
6. "Scope" 패널에서 지역 변수 `a`와 `b`를 확인합니다.
7. "다음 함수 호출로 건너뛰기" 버튼으로 단계별 실행을 합니다.
8. `return` 문에서 버그를 찾습니다.
9. 코드를 수정합니다:
```javascript
function buggyMultiply(a, b) {
    return a * b; // 수정됨!
}

let result = buggyMultiply(5, 3);
console.log('결과:', result);
```

수정된 출력:
```
결과: 15
```

## 심층 탐구
디버깅 개념은 컴퓨팅의 초기 시절부터 있었습니다—1940년대 컴퓨터에서 나방이 발견되었다는 전설에서 시작되었다고 합니다! 오늘날에는 내장된 브라우저 도구(Chrome DevTools, Firefox Developer Tools)나 IDE 통합 디버거(Visual Studio Code, WebStorm)와 같은 JavaScript 디버거들이 다양한 기능을 제공합니다.

내장 디버거의 대안으로는 WebStorm과 같은 제3자 도구 사용이나 변수 상태를 출력하기 위한 좋은 구식 `console.log` 사용 등이 있습니다. 하지만 이러한 방법들은 디버거가 제공하는 실시간 상호 작용과 자세한 검사 기능을 제공하지 않습니다.

구현 세부 사항에 관해서는 대부분의 디버거가 비슷하게 작동합니다: 실행을 일시 중지하게 하는 중단점을 설정할 수 있게 해주고, 코드를 단계별로 진행하며, 현재 변수 상태를 검사하고, 표현식을 관찰하며, 심지어 다른 시나리오를 테스트하기 위해 현장에서 값을 조작하는 것까지 가능하게 합니다.

## 참고자료
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - 디버깅](https://code.visualstudio.com/docs/editor/debugging)
