---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:18:50.124702-07:00
description: "\uC5B4\uB5BB\uAC8C: Google Apps Script\uC5D0\uC11C JSON\uC744 \uB2E4\
  \uB8E8\uB294 \uAC83\uC740 JavaScript\uAC00 JSON \uD30C\uC2F1\uACFC \uBB38\uC790\uC5F4\
  \uD654\uB97C \uC704\uD55C \uB124\uC774\uD2F0\uBE0C \uC9C0\uC6D0\uC744 \uC81C\uACF5\
  \uD558\uAE30 \uB54C\uBB38\uC5D0 \uB300\uCCB4\uB85C \uAC04\uB2E8\uD55C \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uC5EC\uAE30 \uBA87 \uAC00\uC9C0 \uC77C\uBC18\uC801\uC778 \uC791\
  \uC5C5\uC774 \uC788\uC2B5\uB2C8\uB2E4: **1. JSON \uD30C\uC2F1**: \uC6F9 \uC11C\uBE44\
  \uC2A4\uC5D0\uC11C JSON\u2026"
lastmod: '2024-03-13T22:44:54.566583-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740\
  \ JavaScript\uAC00 JSON \uD30C\uC2F1\uACFC \uBB38\uC790\uC5F4\uD654\uB97C \uC704\
  \uD55C \uB124\uC774\uD2F0\uBE0C \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD558\uAE30 \uB54C\
  \uBB38\uC5D0 \uB300\uCCB4\uB85C \uAC04\uB2E8\uD55C \uACFC\uC815\uC785\uB2C8\uB2E4\
  ."
title: "JSON\uACFC \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 38
---

## 어떻게:
Google Apps Script에서 JSON을 다루는 것은 JavaScript가 JSON 파싱과 문자열화를 위한 네이티브 지원을 제공하기 때문에 대체로 간단한 과정입니다. 여기 몇 가지 일반적인 작업이 있습니다:

**1. JSON 파싱**: 웹 서비스에서 JSON 문자열을 검색한다고 가정할 때, 데이터 조작을 위해 JavaScript 객체로 파싱하는 것이 필수입니다.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // 출력: Sample Project
```

**2. JavaScript 객체 문자열화**: 반대로, JavaScript 객체를 JSON 문자열로 변환하는 것은 Apps Script에서 외부 서비스로 데이터를 전송해야 할 때 유용합니다.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // 출력: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. 복잡한 데이터 다루기**:
객체의 배열과 같은 더 복잡한 데이터 구조에 대해서는, 과정은 동일하게 남아 있어 데이터 표현을 위한 JSON의 유연성을 보여줍니다.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // 출력: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## 심층 분석
현대 웹 애플리케이션에서 JSON의 만연함은 과소평가될 수 없으며, 그것의 단순성과 JavaScript, 즉 웹의 언어와의 매끄러운 통합 때문에 뿌리 깊습니다. JavaScript 객체 리터럴에서 영감을 받았으나 좀 더 엄격한 그것의 디자인은 신속한 도입을 촉진합니다. 2000년대 초, JSON은 AJAX 기반 웹 애플리케이션을 위한 XML 대안으로 인기를 얻으며, 더 경량이면서 간결한 데이터 교환 포맷을 제공했습니다. 다양한 Google API와 외부 서비스와의 깊은 통합을 가진 Google Apps Script에서, JSON은 이러한 플랫폼 간 데이터의 구조화, 전송, 및 조작을 위한 중심적인 포맷으로 작용합니다.

웹 애플리케이션에 있어 JSON이 최고를 차지하는 동안, 구성 파일을 위한 YAML이나 고성능 환경에서 더 효율적인 이진 직렬화를 위한 Protobuf와 같은 대체 데이터 포맷들이 존재합니다. 하지만, JSON의 가독성, 사용의 용이성 그리고 프로그래밍 언어 및 도구 전반에 걸친 광범위한 지원의 균형은 Google Apps Script를 비롯한 많은 개발자들에게 기본 선택으로 자리매김하게 합니다.
