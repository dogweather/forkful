---
title:                "yaml으로 작업하기"
html_title:           "Javascript: yaml으로 작업하기"
simple_title:         "yaml으로 작업하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML을 사용하는 이유는 프로그래밍 시 간결하고 읽기 쉬운 형식으로 데이터를 정리할 수 있기 때문입니다.

## 어떻게

YAML을 사용하려면 우선 YAML 라이브러리를 다운로드해야 합니다. 그리고 다음과 같이 코드를 작성해 보세요.

```Javascript
const yaml = require('yaml');

// YAML 파일 불러오기
const data = yaml.parseFile('example.yaml');

// YAML 파일 내용 출력
console.log(data);
```

위 코드를 실행하면 YAML 형식으로 작성된 파일인 'example.yaml'의 내용이 출력됩니다. 이외에도 YAML을 사용하여 JSON 데이터를 변환하거나, JavaScript 객체를 YAML 포맷으로 저장하는 등 다양한 작업을 할 수 있습니다.

## 깊게 파보기

YAML은 주석을 사용할 수 있기 때문에 코드를 문서화하는 데 유용합니다. 또한 커스텀 타입을 정의할 수 있어서 데이터를 더 유연하게 저장할 수 있습니다.

## 더 알아보기

[YAML 공식 문서](https://yaml.org/) <br/>
[YAML vs JSON](https://stackoverflow.com/questions/1726802/json-vs-yaml-which-is-better-for-config-files) <br/>
[YAML 파일 사용 예제](https://www.tutorialspoint.com/yaml/index.htm)