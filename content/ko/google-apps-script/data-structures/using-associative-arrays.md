---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:59.111208-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Google Apps Script\uC5D0\uC11C\uB294 \uC911\
  \uAD04\uD638 `{}`\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC5F0\uAD00 \uBC30\uC5F4(\uAC1D\
  \uCCB4)\uC744 \uC0DD\uC131\uD558\uACE0 \uC870\uC791\uD558\uBA70, \uC774 \uC548\uC5D0\
  \ \uD0A4-\uAC12 \uC30D\uC744 \uC815\uC758\uD569\uB2C8\uB2E4. \uD0A4\uB294 \uACE0\
  \uC720 \uC2DD\uBCC4\uC790\uC774\uBA70, \uAC12\uC740 \uBB38\uC790\uC5F4\uACFC \uC22B\
  \uC790\uBD80\uD130 \uB354 \uBCF5\uC7A1\uD55C \uAC1D\uCCB4\uB098 \uD568\uC218\uC5D0\
  \ \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uAE30\uBCF8 \uC608\uB294 \uB2E4\uC74C\uACFC\u2026"
lastmod: '2024-03-13T22:44:54.516788-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C\uB294 \uC911\uAD04\uD638 `{}`\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uC5F0\uAD00 \uBC30\uC5F4(\uAC1D\uCCB4)\uC744 \uC0DD\uC131\uD558\
  \uACE0 \uC870\uC791\uD558\uBA70, \uC774 \uC548\uC5D0 \uD0A4-\uAC12 \uC30D\uC744\
  \ \uC815\uC758\uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 사용 방법:
Google Apps Script에서는 중괄호 `{}`를 사용하여 연관 배열(객체)을 생성하고 조작하며, 이 안에 키-값 쌍을 정의합니다. 키는 고유 식별자이며, 값은 문자열과 숫자부터 더 복잡한 객체나 함수에 이르기까지 다양할 수 있습니다. 기본 예는 다음과 같습니다:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // 값에 접근하기
  Logger.log(user.name); // 출력: John Doe
  Logger.log(user["email"]); // 출력: johndoe@example.com

  // 새로운 키-값 쌍 추가하기
  user.title = "소프트웨어 개발자";
  user["country"] = "USA";

  Logger.log(user.title); // 출력: 소프트웨어 개발자

  // 키-값 쌍을 순회하기
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

순회 부분의 예제 출력은 다음과 같을 수 있습니다:
```
name: John Doe
age: 30
email: johndoe@example.com
title: 소프트웨어 개발자
country: USA
```

속성에 접근하고 설정하기 위해 점 표기법과 대괄호 표기법을 모두 사용할 수 있음을 주목하세요. 대괄호 표기법은 키가 동적으로 결정되거나 식별자에서 허용되지 않는 문자를 포함할 경우 특히 유용합니다.

## 심층 분석
객체 형태의 연관 배열은 JavaScript의 프로토타입 기반 상속 메커니즘을 반영하면서 JavaScript 및 확장된 Google Apps Script의 핵심 요소가 되었습니다. 전통적인 연관 배열이나 딕셔너리(예: Python의 dict)를 가진 언어와 달리, Google Apps Script 객체는 데이터를 구조화하는 유연하고 강력한 수단을 제공하며, JavaScript의 동적 특성으로부터 이점을 얻습니다.

그러나 ECMAScript 2015 사양이 `Map`과 `Set` 객체를 도입하여 삽입 순서를 유지하고 큰 데이터셋에 대한 성능이 더 우수한 등 객체에 비해 특정 이점이 있는 보다 직관적인 연관 컬렉션 처리를 제공한다는 점을 주목하는 것이 중요합니다. Google Apps Script도 이들을 지원하지만 객체를 사용하거나 `Map`/`Set` 구조 중에서 선택하는 것은 특정 필요성과 성능 고려 사항에 따라 달라집니다. 대부분의 연관 배열 작업에 대해 전통적인 객체 기반 구현은 친숙하고 다양하게 접근할 수 있는 방법을 제공하지만, 스크립트의 복잡성이 증가함에 따라 새로운 대안을 검토하는 것이 좋습니다.
