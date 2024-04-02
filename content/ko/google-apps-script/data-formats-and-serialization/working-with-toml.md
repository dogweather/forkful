---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:18.689879-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C\
  , \uBA85\uD655\uD55C \uC758\uBBF8\uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\
  \uC6B4 \uAD6C\uC131 \uD30C\uC77C \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB2E4\uC591\uD55C \uD658\uACBD\uC5D0\uC11C \uC751\uC6A9\
  \ \uD504\uB85C\uADF8\uB7A8 \uC124\uC815\uACFC \uAD6C\uC131\uC744 \uC6D0\uD65C\uD558\
  \uAC8C \uAD00\uB9AC\uD560 \uC218 \uC788\uB3C4\uB85D \uC9C1\uAD00\uC801\uC774\uACE0\
  \ \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uD615\uD0DC\uB85C \uB418\uC5B4\
  \ \uC788\uAE30\u2026"
lastmod: '2024-03-13T22:44:54.569689-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C, \uBA85\
  \uD655\uD55C \uC758\uBBF8\uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4\
  \ \uAD6C\uC131 \uD30C\uC77C \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB2E4\uC591\uD55C \uD658\uACBD\uC5D0\uC11C \uC751\uC6A9 \uD504\
  \uB85C\uADF8\uB7A8 \uC124\uC815\uACFC \uAD6C\uC131\uC744 \uC6D0\uD65C\uD558\uAC8C\
  \ \uAD00\uB9AC\uD560 \uC218 \uC788\uB3C4\uB85D \uC9C1\uAD00\uC801\uC774\uACE0 \uC0AC\
  \uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uD615\uD0DC\uB85C \uB418\uC5B4 \uC788\
  \uAE30\u2026"
title: "TOML\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 39
---

## 무엇 & 왜?

TOML은 Tom's Obvious, Minimal Language의 약자로, 명확한 의미론으로 인해 읽기 쉬운 구성 파일 형식입니다. 프로그래머들은 다양한 환경에서 응용 프로그램 설정과 구성을 원활하게 관리할 수 있도록 직관적이고 사람이 읽을 수 있는 형태로 되어 있기 때문에, 응용 프로그램의 구성 파일로 자주 사용합니다.

## 방법:

Google Apps Script는 본질적으로 Google 앱 제품군에 접근할 수 있는 JavaScript이므로, Google Apps Script 내에서 직접 TOML을 작업하는 것은 약간의 창의성을 요구합니다. Google Apps Script는 기본적으로 TOML 파싱을 지원하지 않지만, JavaScript 라이브러리를 활용하거나 기본적인 요구 사항에 대해 간단한 파서를 작성하여 사용할 수 있습니다.

예를 들어 간단한 TOML 구성 문자열을 파싱해 봅시다:

```javascript
// TOML 문자열
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// 간단한 TOML to JSON 파서 함수
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // 새 섹션
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // 간단함을 위해 eval 사용; 실제 코드에서는 주의 필요
      currentSection[key] = value;
    }
  });
  return result;
}

// 파서 테스트
var configObject = parseTOML(tomlString);
console.log(configObject);

```

`console.log`에서 샘플 출력은 JSON 객체처럼 보여, Google Apps Script 내에서 구성 속성에 쉽게 접근할 수 있게 해줍니다:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## 심층 분석

TOML은 GitHub의 창립자 중 한명인 Tom Preston-Werner가 만들었으며, 구성 파일에 대해 JSON보다 인간 친화적인 동시에 모호하지 않게 파싱될 수 있는 능력을 유지하려고 합니다. 가능한 한 단순하게 유지하는 것이 목표이며, 이는 코드베이스에서 단순성과 가독성을 추구하는 많은 개발 프로젝트의 정신과 잘 어울립니다.

Google Apps Script의 맥락에서, TOML 사용은 직접 지원 부족과 수동으로 또는 제3자 라이브러리를 통한 파싱이 필요하다는 점에서 일부 오버헤드를 도입합니다. Google의 생태계와 깊게 통합되어 있지 않은 더 작은 프로젝트나 경우에는 JSON이나 심지어 스크립트 속성 내의 단순한 키-값 쌍 구조가 충분하고 구현하기 더 직관적일 수 있습니다. 그러나 인간 친화적인 구성 파일을 우선시하고 이미 TOML에 전념하는 응용 프로그램의 경우, 사용자 정의 스크립트를 통한 TOML 파싱 통합은 선호하는 구성 패러다임에서 벗어나지 않으면서 유연성과 유지보수성에 유용한 계층을 추가합니다.
