---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:44.559585-07:00
description: "\uBC29\uBC95: Google Apps Script\uB294 \uD604\uB300\uC801\uC778 JavaScript\uB97C\
  \ \uAE30\uBC18\uC73C\uB85C \uD558\uACE0 \uC788\uC73C\uBA70, `substring()`, `substr()`,\
  \ `slice()`\uB97C \uD3EC\uD568\uD55C \uC5EC\uB7EC \uAC00\uC9C0 \uBC29\uBC95\uC744\
  \ \uD1B5\uD574 \uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC744 \uB2EC\uC131\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uAC01\uAC01\uC740 \uADF8 \uB098\uB984\uC758\
  \ \uBBF8\uBB18\uD55C \uCC28\uC774\uAC00 \uC788\uC9C0\uB9CC,\u2026"
lastmod: '2024-03-13T22:44:54.510778-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uB294 \uD604\uB300\uC801\uC778 JavaScript\uB97C \uAE30\
  \uBC18\uC73C\uB85C \uD558\uACE0 \uC788\uC73C\uBA70, `substring()`, `substr()`, `slice()`\uB97C\
  \ \uD3EC\uD568\uD55C \uC5EC\uB7EC \uAC00\uC9C0 \uBC29\uBC95\uC744 \uD1B5\uD574 \uC11C\
  \uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC744 \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
weight: 6
---

## 방법:
Google Apps Script는 현대적인 JavaScript를 기반으로 하고 있으며, `substring()`, `substr()`, `slice()`를 포함한 여러 가지 방법을 통해 서브스트링 추출을 달성할 수 있습니다. 각각은 그 나름의 미묘한 차이가 있지만, 모두 문자열에서 특정 문자를 추출하는 목적을 제공합니다.

```javascript
// substring() 사용 예
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // 출력: Hello

// substr() 사용 예
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // 출력: world

// slice() 사용 예
var resultSlice = str.slice(-6);
console.log(resultSlice); // 출력: world!
```

각 방법은 시작 위치와, `slice()`가 끝에서부터 시작하기 위해 음수 인덱스를 받아들일 수 있지만, 끝 위치나 추출할 문자 수를 제외하고 두 인수를 받아들입니다. 이러한 작업 후에 원래 문자열이 변경되지 않고 새 문자열 값을 반환하는 것에 주목할 가치가 있습니다.

## 심층 분석
역사적으로, 서브스트링을 추출하기 위한 JavaScript 방법들은 이름과 기능면에서 유사하여 혼동의 원인이 되어왔습니다. 그러나 Google Apps Script와 현대적인 JavaScript에서는 `substring()`과 `slice()`가 가장 자주 사용되며, `substr()`은 더 이상 사용되지 않는 것으로 간주되고 있습니다. 이것은 앞으로 지속될 코드를 작성하는 사람들에게 중요합니다.

`substring()`과 `slice()` 사이의 주요 차이점은 음수 인덱스를 어떻게 다루느냐입니다; `substring()`은 음수 인덱스를 0으로 간주하는 반면, `slice()`는 음수 인덱스를 받아들여 문자열의 끝에서 추출을 시작할 수 있습니다. 이러한 특성은 문자열의 정확한 길이를 모르거나 끝에서 추출해야 할 경우에 특히 유용합니다.

서브스트링 추출에 어떤 방법을 사용할지 결정할 때, 선택은 종종 작업의 특정 요구사항(예: 음수 인덱스를 처리하는 것이 유익한지 여부) 및 개인이나 팀 코딩 표준에 따라 달라집니다. 일률적으로 최고의 방법이 있는 것은 아니지만, 미묘한 차이와 성능 함축을 이해하는 것은 정보에 입각한 결정을 내리는 데 도움이 될 수 있습니다.
