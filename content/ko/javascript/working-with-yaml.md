---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 쓰나?)
YAML은 데이터를 인간이 읽을 수 있는 형태로 표현하는데 사용합니다. 설정 파일, 데이터 교환 등의 경우에 JSON이나 XML보다 간결하고 읽기 쉬워서 많은 프로그래머들이 사용합니다.

## How to: (방법:)
```Javascript
// js-yaml 라이브러리 설치 필요
const yaml = require('js-yaml');
const fs = require('fs');

// YAML 파일 읽기
let config;
try {
  config = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
  console.error(e);
}

// 자바스크립트 객체를 YAML로 변환
const data = { name: '홍길동', age: 30 };
const yamlData = yaml.dump(data);
console.log(yamlData);
```
출력 예시:
```plaintext
{ name: '홍길동', age: 30 }
name: 홍길동
age: 30
```

## Deep Dive (심층 분석)
- **역사적 맥락**: YAML은 "YAML Ain't Markup Language"의 재귀적 약어로, 2001년에 개발되었습니다. 
- **대안들**: JSON, XML과 같은 형식이 있지만, YAML은 가독성과 편집의 용이성에서 더 나은 선택이 될 수 있습니다.
- **구현 세부사항**: YAML에서 탭 대신 공백을 사용하는 것은 중요한 규칙이며, 객체와 리스트를 나타내는 방법에 있어서 매우 직관적입니다.

## See Also (참고자료)
- YAML 공식 문서: https://yaml.org/spec/
- js-yaml GitHub 페이지: https://github.com/nodeca/js-yaml
- JSON과 YAML의 비교: https://www.json2yaml.com/
