---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:32.798122-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4 \uB610\uB294 JavaScript\uC5D0\uC11C \uB354\
  \ \uC815\uD655\uD558\uAC8C \uC54C\uB824\uC9C4 \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\
  \uBA74 \uD0A4\uC640 \uAC12\uC744 \uB9E4\uD551\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uC774\uB294 \uD2B9\uC815 \uC774\uB984(\uD0A4)\uC744 \uD1B5\uD574 \uC694\uC18C\
  \uC5D0 \uC811\uADFC\uD558\uAE38 \uC6D0\uD560 \uB54C \uC720\uC6A9\uD558\uBA70, \uC774\
  \uB294 \uCF54\uB4DC\uB97C \uB354 \uC77D\uAE30 \uC27D\uACE0 \uC720\uC5F0\uD558\uAC8C\
  \ \uB9CC\uB4ED\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.780798-06:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4 \uB610\uB294 JavaScript\uC5D0\uC11C \uB354 \uC815\
  \uD655\uD558\uAC8C \uC54C\uB824\uC9C4 \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uBA74\
  \ \uD0A4\uC640 \uAC12\uC744 \uB9E4\uD551\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4.\
  \ \uC774\uB294 \uD2B9\uC815 \uC774\uB984(\uD0A4)\uC744 \uD1B5\uD574 \uC694\uC18C\
  \uC5D0 \uC811\uADFC\uD558\uAE38 \uC6D0\uD560 \uB54C \uC720\uC6A9\uD558\uBA70, \uC774\
  \uB294 \uCF54\uB4DC\uB97C \uB354 \uC77D\uAE30 \uC27D\uACE0 \uC720\uC5F0\uD558\uAC8C\
  \ \uB9CC\uB4ED\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 무엇 & 왜?

연관 배열 또는 JavaScript에서 더 정확하게 알려진 객체를 사용하면 키와 값을 매핑할 수 있습니다. 이는 특정 이름(키)을 통해 요소에 접근하길 원할 때 유용하며, 이는 코드를 더 읽기 쉽고 유연하게 만듭니다.

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
