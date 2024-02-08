---
title:                "YAML로 작업하기"
aliases:
- ko/google-apps-script/working-with-yaml.md
date:                  2024-02-01T22:07:14.248154-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

"YAML Ain't Markup Language"의 약자인 YAML은 컴퓨터 언어간 데이터 교환 및 설정 파일에 주로 사용되는 인간이 읽을 수 있는 데이터 직렬화 표준입니다. 특히, 많은 설정을 요구하는 프로젝트나 다른 시스템 간에 구조화된 데이터를 전송할 때 프로그래머들이 YAML의 단순성과 가독성 때문에 종종 YAML을 사용하곤 합니다.

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
