---
title:                "JSON과 함께 작업하기"
aliases:
- ko/google-apps-script/working-with-json.md
date:                  2024-02-01T22:18:50.124702-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON, 즉 JavaScript Object Notation은 서버에서 클라이언트로의 통신과 설정 파일을 위해 이상적인 경량형 데이터 저장 및 전송 포맷입니다. 프로그래머들은 Google Apps Script에서 Google 서비스(예: Sheets, Docs, Drive)와 외부 소스 간의 원활한 데이터 교환을 위해, 그것의 인간이 읽을 수 있는 구조와 JavaScript 기반 환경 내에서의 쉬운 통합 때문에 JSON을 활용합니다.

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
