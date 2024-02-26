---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:59.111208-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Google Apps Script(JavaScript\uC758\
  \ \uD55C \uBCC0\uC885)\uC5D0\uC11C \uAC1D\uCCB4\uB85C \uC54C\uB824\uC838 \uC788\uC73C\
  \uBA70, \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uD0A4-\uAC12 \uC30D\uC758 \uCEEC\uB809\
  \uC158\uC744 \uC0DD\uC131\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uC774\
  \ \uAE30\uB2A5\uC740 \uD2B9\uD788 \uB3D9\uC801\uC73C\uB85C \uBA85\uBA85\uB41C \uC18D\
  \uC131\uC744 \uB2E4\uB8F0 \uB54C\uB098 \uC804\uD1B5\uC801\uC778 \uBC30\uC5F4\uC758\
  \ \uC120\uD615 \uC800\uC7A5 \uBC0F \uC811\uADFC \uBAA8\uB378\uC774 \uCDA9\uBD84\uD558\
  \uC9C0 \uC54A\uC744\u2026"
lastmod: '2024-02-25T18:49:51.558333-07:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Google Apps Script(JavaScript\uC758 \uD55C\
  \ \uBCC0\uC885)\uC5D0\uC11C \uAC1D\uCCB4\uB85C \uC54C\uB824\uC838 \uC788\uC73C\uBA70\
  , \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uD0A4-\uAC12 \uC30D\uC758 \uCEEC\uB809\uC158\
  \uC744 \uC0DD\uC131\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uC774 \uAE30\
  \uB2A5\uC740 \uD2B9\uD788 \uB3D9\uC801\uC73C\uB85C \uBA85\uBA85\uB41C \uC18D\uC131\
  \uC744 \uB2E4\uB8F0 \uB54C\uB098 \uC804\uD1B5\uC801\uC778 \uBC30\uC5F4\uC758 \uC120\
  \uD615 \uC800\uC7A5 \uBC0F \uC811\uADFC \uBAA8\uB378\uC774 \uCDA9\uBD84\uD558\uC9C0\
  \ \uC54A\uC744\u2026"
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나?

연관 배열은 Google Apps Script(JavaScript의 한 변종)에서 객체로 알려져 있으며, 프로그래머가 키-값 쌍의 컬렉션을 생성할 수 있게 해줍니다. 이 기능은 특히 동적으로 명명된 속성을 다룰 때나 전통적인 배열의 선형 저장 및 접근 모델이 충분하지 않을 때, 데이터를 효율적으로 저장하고 조작하는 데 중추적입니다.

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
