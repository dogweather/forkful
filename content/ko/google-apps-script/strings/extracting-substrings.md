---
aliases:
- /ko/google-apps-script/extracting-substrings/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:44.559585-07:00
description: "\uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC740 \uBB38\uC790\uC5F4\
  \uC758 \uC77C\uBD80\uBD84\uC744 \uAC00\uC838\uC640 \uBCF8\uC9C8\uC801\uC73C\uB85C\
  \ \uAE30\uC874 \uBB38\uC790\uC5F4\uC758 \uC77C\uBD80\uB85C\uBD80\uD130 \uC0C8\uB85C\
  \uC6B4 \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uD30C\
  \uC2F1, \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uB97C \uC704\uD55C \uD14D\
  \uC2A4\uD2B8 \uC870\uC791 \uB610\uB294 \uB2E4\uC591\uD55C \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC758 \uC785\uB825 \uCC98\uB9AC\uB97C \uD3EC\uD568\uD55C \uB2E4\uC591\
  \uD55C \uC774\uC720\uB85C \uC774\uB97C \uC218\uD589\uD558\uB294\uB370, \uC774\uB294\
  \u2026"
lastmod: 2024-02-18 23:09:05.536549
model: gpt-4-0125-preview
summary: "\uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC740 \uBB38\uC790\uC5F4\uC758\
  \ \uC77C\uBD80\uBD84\uC744 \uAC00\uC838\uC640 \uBCF8\uC9C8\uC801\uC73C\uB85C \uAE30\
  \uC874 \uBB38\uC790\uC5F4\uC758 \uC77C\uBD80\uB85C\uBD80\uD130 \uC0C8\uB85C\uC6B4\
  \ \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uD30C\uC2F1\
  , \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uB97C \uC704\uD55C \uD14D\uC2A4\
  \uD2B8 \uC870\uC791 \uB610\uB294 \uB2E4\uC591\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC758 \uC785\uB825 \uCC98\uB9AC\uB97C \uD3EC\uD568\uD55C \uB2E4\uC591\uD55C\
  \ \uC774\uC720\uB85C \uC774\uB97C \uC218\uD589\uD558\uB294\uB370, \uC774\uB294\u2026"
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?

서브스트링 추출은 문자열의 일부분을 가져와 본질적으로 기존 문자열의 일부로부터 새로운 문자열을 생성하는 것을 말합니다. 프로그래머들은 데이터 파싱, 사용자 인터페이스를 위한 텍스트 조작 또는 다양한 애플리케이션의 입력 처리를 포함한 다양한 이유로 이를 수행하는데, 이는 서브스트링 추출을 스크립팅 무기고에서 다재다능한 도구로 만듭니다.

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
