---
date: 2024-01-26 04:23:31.927660-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: JavaScript\uC5D0\uC11C TOML\uC744 \uC0AC\uC6A9\
  \uD558\uAE30 \uC704\uD574\uC120 `@iarna/toml`\uACFC \uAC19\uC740 \uD30C\uC11C\uAC00\
  \ \uD544\uC694\uD569\uB2C8\uB2E4. \uC6B0\uC120 \uC124\uCE58\uD558\uC2ED\uC2DC\uC624\
  : `npm install @iarna/toml`. \uADF8 \uB2E4\uC74C, TOML \uBB38\uC790\uC5F4\uC744\
  \ JavaScript \uAC1D\uCCB4\uB85C \uD30C\uC2F1\uD558\uAC70\uB098 JavaScript \uAC1D\
  \uCCB4\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.825648-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\uC5D0\uC11C TOML\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574\uC120\
  \ `@iarna/toml`\uACFC \uAC19\uC740 \uD30C\uC11C\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4\
  ."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 사용 방법:
JavaScript에서 TOML을 사용하기 위해선 `@iarna/toml`과 같은 파서가 필요합니다. 우선 설치하십시오: `npm install @iarna/toml`. 그 다음, TOML 문자열을 JavaScript 객체로 파싱하거나 JavaScript 객체를 TOML 형식으로 문자열화하십시오.

```javascript
const toml = require('@iarna/toml');

// TOML 문자열을 JS 객체로 파싱
const tomlStr = `
title = "TOML 예제"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// JS 객체를 TOML 문자열로 변환
const jsObject = {
  title: "TOML 예제",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## 심층 탐구
TOML은 2013년 GitHub의 공동 창업자인 Tom Preston-Werner에 의해 처음 발표되었습니다. INI와 같은 다른 형식을 대체하기 위해 더 표준화되고 파싱하기 쉽도록 설계되었습니다. JSON과 YAML은 대안이지만 너무 복잡하거나 유연할 수 있습니다. TOML의 장점은 간단하고 명확한 형식을 선호하는 정적 구성에서 있습니다. 그 설계는 해시 테이블로의 직관적인 매핑을 허용하며, 키와 값은 속성 이름과 그 값에 해당합니다. 보다 넓은 채택을 위해서는 다양한 생태계 지원으로 인해 TOML과 다른 형식 간에 변환할 수 있는 도구를 통합해야 할 수도 있습니다.

## 또한 보기
- 공식 TOML GitHub 저장소: https://github.com/toml-lang/toml
- TOML vs. YAML vs. JSON 비교: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` 패키지: https://www.npmjs.com/package/@iarna/toml
