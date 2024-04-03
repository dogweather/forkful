---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:32.798122-07:00
description: "\uBC29\uBC95: JavaScript\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4(\uAC1D\
  \uCCB4)\uC744 \uC0DD\uC131\uD558\uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uAC04\
  \uB2E8\uD569\uB2C8\uB2E4. \uC911\uAD04\uD638 `{}`\uB85C \uAC1D\uCCB4\uB97C \uC815\
  \uC758\uD558\uACE0, \uADF8 \uC548\uC5D0\uC11C \uD0A4-\uAC12 \uC30D\uC758 \uC9D1\uD569\
  \uC744 \uC815\uC758\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD0A4\uB294 \uD56D\uC0C1\
  \ \uBB38\uC790\uC5F4\uC774\uACE0, \uAC12\uC740 \uBB38\uC790\uC5F4, \uC22B\uC790\
  , \uBC30\uC5F4, \uC2EC\uC9C0\uC5B4 \uB2E4\uB978 \uAC1D\uCCB4\uC77C \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.780798-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4(\uAC1D\uCCB4)\uC744 \uC0DD\
  \uC131\uD558\uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uAC04\uB2E8\uD569\uB2C8\
  \uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
JavaScript에서 연관 배열(객체)을 생성하고 사용하는 것은 간단합니다. 중괄호 `{}`로 객체를 정의하고, 그 안에서 키-값 쌍의 집합을 정의할 수 있습니다. 키는 항상 문자열이고, 값은 문자열, 숫자, 배열, 심지어 다른 객체일 수 있습니다.

```javascript
// 연관 배열 생성
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// 요소 접근
console.log(userInfo.name); // 출력: Alex
console.log(userInfo["email"]); // 출력: alex@example.com

// 새로운 요소 추가
userInfo.job = "개발자";
userInfo["국가"] = "캐나다";

console.log(userInfo);
/* 출력:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "개발자",
  국가: "캐나다"
}
*/

// 요소 삭제
delete userInfo.age;
console.log(userInfo);
/* 출력:
{
  name: "Alex",
  email: "alex@example.com",
  job: "개발자",
  국가: "캐나다"
}
*/
```

보시다시피, 연관 배열에서 요소에 접근하거나 추가하거나 삭제하는 것은 직관적이고 단순합니다.

## 심층 분석
JavaScript 세계에서 "연관 배열"이라는 용어를 자주 듣지만, JavaScript에는 다른 언어들(예: PHP)처럼 진정한 연관 배열이 없기 때문에 기술적으로 부정확한 용어입니다. JavaScript가 가진 것은 유사한 목적을 제공하지만 더 강력하고 유연한 구조인 객체입니다.

역사적으로, 프로그래밍 언어에서 배열은 숫자 인덱스로 접근되는 항목들의 컬렉션을 보관하기 위해 설계되었습니다. 하지만, 소프트웨어 개발이 진화하면서 더 유연한 데이터 구조가 필요하게 되었습니다. 연관 배열 또는 다른 언어의 사전은 이러한 반응 중 하나로, 임의의 키를 통해 요소에 접근할 수 있도록 했습니다.

객체를 키-값 저장소로 사용하는 JavaScript의 접근 방식은 기능성의 혼합을 제공합니다. 이는 속성(키)을 이름으로 추가, 제거 및 조회할 수 있게 합니다. JSON(JavaScript Object Notation)은 이 구조의 유틸리티를 증명하며, 웹상의 데이터 교환 표준이 되었습니다.

객체가 연관 배열의 대부분의 요구를 충족시키지만, 키 순서 또는 반복이 중요한 경우에는 ES6에서 도입된 `Map` 객체가 더 나은 대안을 제공합니다. `Map`은 키 순서를 유지하고, 키로서 더 넓은 범위의 데이터 유형을 수용하며, 반복 및 크기 검색에 유용한 메서드를 포함하고 있습니다. 이런 장점에도 불구하고, 전통적인 객체 구문은 많은 일반적인 시나리오에서 그 간결함과 사용의 용이성으로 인해 인기를 유지하고 있습니다.
