---
title:                "디버그 출력 출력하기"
aliases:
- ko/google-apps-script/printing-debug-output.md
date:                  2024-02-01T21:58:13.284859-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버그 출력 출력하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력을 출력하는 것은 코드에 전략적으로 로그 문을 배치하여 런타임 동안 변수 값, 실행 흐름 또는 메시지 오류를 표시하는 것을 포함합니다. 프로그래머들은 그들의 스크립트의 동작을 추적하고 진단하여, 그들의 Google Apps Script 애플리케이션의 정확성과 효율성을 보장하기 위해 이를 광범위하게 활용합니다.

## 방법:

Google Apps Script는 기본 디버깅에 `Logger` 클래스를 제공하고, 더 고급 필요에는 V8 런타임에서 소개된 `console` 클래스를 제공합니다.

**Logger 사용하기:**

Logger 클래스를 사용하면 디버그 메시지를 로그할 수 있으며, 실행 후 Apps Script 편집기에서 `보기 > 로그`로 이를 볼 수 있습니다. 간단한 예는 다음과 같습니다:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

`logSample()`을 실행한 후, 로그 뷰어에서 "Hello, Wired Reader!"라는 로그를 볼 수 있습니다.

**V8 런타임에서 console.log 사용하기:**

V8 런타임에서는 `console.log`가 다른 언어에서 온 개발자들에게 더 익숙한 문법을 제공합니다:

```javascript
function consoleSample() {
  var status = 'active';
  var count = 150;
  console.log(`Current status: ${status}, Count: ${count}`);
}
```

실행 후, `보기 > Stackdriver 로깅`으로 접근하여 출력을 볼 수 있습니다. 이는 문자열 보간 및 객체 검사를 지원하며, Google Cloud의 로깅과 통합하여 영구 로그와 고급 필터링 기능을 제공하는 더욱 강력한 기능입니다.

**console.log에서의 샘플 출력:**

```
Current status: active, Count: 150
```

## 심층 분석

최초에, `Logger.log`는 Google Apps Script에서 디버깅을 위한 주요 도구였으며, 검사를 위해 출력을 인쇄하는 간단하고 직선적인 방법을 제공했습니다. 그러나 스크립트가 더 복잡해지고 Google Cloud Platform 서비스와 통합됨에 따라, 더 강력한 로깅 솔루션이 필요하다는 것이 분명해졌습니다.

V8 런타임이 도입되면서, `console.log`가 추가되었습니다. 이는 Google Apps Script를 표준 JavaScript 문법과 일치시켜, JavaScript에 익숙한 개발자들에게 언어를 더 접근하기 쉽게 만들뿐만 아니라, Google Cloud의 강력한 로깅 인프라를 활용합니다. `console.log`의 도입과 그것의 Google Cloud Platform과의 통합은 Google Apps Script 내에서 디버깅 기능의 중요한 진화를 표시하며, 개발자들에게 스크립트를 모니터링하고 문제를 해결하는 데 있어 더욱 동적이고 확장 가능한 접근 방식을 제공합니다.

`Logger.log`는 기본 디버깅 요구 사항과 소규모 프로젝트에 충분하지만, V8 런타임을 사용하는 `console.log`는 실행 세션을 넘어 로그를 유지하고, Google Cloud 콘솔 내에서 로그를 검색하고 필터링할 수 있는 능력과 함께, 현대 JavaScript 개발 관행과 전반적으로 일치하는 보다 포괄적이고 미래 지향적인 솔루션을 제공합니다. 그러나, 개발자는 이러한 옵션 사이에서 선택할 때 자신들의 프로젝트의 복잡성과 규모에 대한 필요를 측정해야 합니다.
