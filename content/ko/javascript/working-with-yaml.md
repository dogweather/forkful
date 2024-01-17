---
title:                "yaml 사용하기"
html_title:           "Javascript: yaml 사용하기"
simple_title:         "yaml 사용하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
YAML 작업이란 무엇인가요? 그리고 왜 프로그래머들은 이 작업을 하나요?
YAML은 'YAML Ain't Markup Language'의 약자로, 데이터의 구조화된 양식을 다루는 데 사용되는 포맷입니다. 프로그래머들이 YAML을 사용하는 이유는 간단한 구조와 읽기 쉬운 문법으로 텍스트 파일을 다루는 데 매우 유용하기 때문입니다.

## 방법:
```Javascript
// YAML 파일을 다룰 때 사용하는 예제 코드
var fs = require('fs');
var yaml = require('js-yaml');

// YAML 파일을 열어서 데이터를 로드하는 함수
function loadYAML(filename) {
  var data = fs.readFileSync(filename, 'utf8');
  return yaml.safeLoad(data);
}

// 새로운 YAML 파일을 생성하는 함수
function createYAML(filename, data) {
  var yamlData = yaml.safeDump(data);
  fs.writeFileSync(filename, yamlData, 'utf8');
}

```
예제 코드를 실행하면, YAML 파일이 생성되고 데이터가 로드되게 됩니다.

## 심층 분석:
### 역사적 배경:
YAML은 2001년 최초로 만들어질 당시 데이터 포맷의 표준이었던 XML보다 간단하고 가독성이 뛰어나기 때문에 빠르게 인기를 얻었습니다. 하지만 XML의 복잡한 스키마를 대체할 수는 없었기 때문에 아직까지도 많은 서비스에서 사용되고 있습니다.

### 대안:
YAML의 대안으로는 JSON이 있습니다. YAML보다 구조가 단순하고 문법도 간단하지만, 읽기 쉬운 YAML과는 다르게 JSON은 작은 따옴표를 사용해야 하기 때문에 코드가 더 복잡해질 수 있습니다.

### 구현 세부사항:
YAML은 Ruby, Python 등의 다양한 언어에서 사용할 수 있도록 라이브러리가 제공되고 있습니다. JavaScript에서 YAML을 다루기 위해서는 'yaml' 라이브러리를 설치하고, 'js-yaml' 패키지를 이용해야 합니다.

## 관련 자료:
- YAML 공식 사이트: https://yaml.org/
- YAML 백서: https://yaml.org/spec/1.2/spec.html
- yaml 라이브러리: https://www.npmjs.com/package/yaml