---
title:                "문자열 대문자화"
aliases:
- /ko/google-apps-script/capitalizing-a-string/
date:                  2024-02-01T21:48:50.897258-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/capitalizing-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열을 대문자화하는 것은 입력값을 수정하여 첫 문자를 대문자로 하고 나머지는 소문자로 유지하는 것을 포함하는데, 보통 이름이나 제목의 서식을 맞추기 위해 사용됩니다. 프로그래머들은 이것을 데이터 일관성을 보장하고 사용자 인터페이스나 문서 내에서 가독성을 향상하기 위해 사용합니다.

## 어떻게 하나요?

Google Apps Script는 JavaScript를 기반으로 하기 때문에 문자열을 대문자화하는 몇 가지 방법을 제공하지만, 내장 함수는 없습니다. 여기 몇 가지 간단한 예제가 있습니다:

**방법 1: charAt()와 slice() 사용하기**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// 샘플 사용
let result = capitalizeString('hello, world');
console.log(result);  // 출력: Hello, world
```

**방법 2: 정규 표현식 사용하기**

보다 우아하게 엣지 케이스를 처리하기 위해 정규 표현식 기반 솔루션을 선호하는 사람들을 위해:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// 샘플 사용
let result = capitalizeStringRegex('hello, world');
console.log(result);  // 출력: Hello, world
```

두 방법 모두 문자열의 첫 문자를 대문자로 하고 나머지는 소문자로 하는 것을 보장하여, Google Sheets 조작이나 Apps Script를 통한 문서 편집을 포함한 다양한 애플리케이션에 적합합니다.

## 심층 분석

Google Apps Script에서 문자열을 대문자화하는 것은 JavaScript의 강력한 문자열 조작 기능을 활용하여 간단합니다. 역사적으로 Python과 같은 언어는 이를 달성하기 위해 `.capitalize()`와 같은 내장 메서드를 제공하며, JavaScript와 Apps Script 프로그래머에게 조금 더 추가적인 단계를 요구합니다. 하지만, JavaScript/Google Apps Script에서 내장 함수가 없는 것은 유연성을 촉진하고 문자열 조작 기술에 대한 더 깊은 이해를 요구합니다.

복잡한 시나리오에서, 예를 들어 문자열의 각 단어를 대문자로 하는 것(Title Case) 같은 경우, 프로그래머는 regex 메서드와 `split()` 및 `map()` 함수를 결합하여 각 단어를 개별적으로 처리할 수 있습니다. 비록 Google Apps Script가 문자열 대문자화를 위한 직접적인 방법을 제공하지 않지만, 기존 JavaScript 문자열 조작 메서드의 사용은 충분한 유연성을 제공하여, 개발자가 그들의 특정한 요구에 따라 문자열을 효과적으로 다룰 수 있게 합니다.

성능과 효율성이 중요한 경우에는, 특히 긴 문자열이나 큰 루프 내에서의 연산에 있어서는 정규 표현식보다 직접적인 문자열 조작이 더 성능이 좋을 수 있음을 언급할 가치가 있습니다. 하지만, Google Apps Script 내에서의 대부분의 실용적인 응용에서는 두 접근 방식 모두 신뢰할 수 있는 솔루션을 제공합니다.
