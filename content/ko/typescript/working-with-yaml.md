---
title:                "TypeScript: Yaml로 작업하기"
simple_title:         "Yaml로 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜
TypeScript를 사용하여 YAML을 다루는 것에 대해 글을 쓰는 이유는 여러분이 개발하거나 유지 보수할 때 YAML 파일을 더 쉽고 효율적으로 다룰 수 있기 때문입니다.

## 어떻게
```TypeScript
// YAML 파일 로드
import YAML from 'yaml';

// YAML 해석
const yamlData = YAML.parse(`
    name: John Smith
    age: 30
    job: Developer
`);

// YAML 데이터 사용
console.log(`제 이름은 ${yamlData.name}이고, ${yamlData.age}살입니다. 제 직업은 ${yamlData.job}입니다.`);
```
결과: 제 이름은 John Smith이고, 30살입니다. 제 직업은 Developer입니다.

## 깊이 파고들기
YAML은 사람들이 쉽게 읽고 작성할 수 있는 형식으로 데이터를 저장할 수 있는 구조적인 형식입니다. TypeScript를 사용하면 YAML 파일을 JSON으로 변환하거나 JSON을 YAML로 변환할 수 있습니다. 또한 라이브러리를 사용하여 YAML 파일을 자유롭게 다룰 수 있습니다.

## 더 보기
- [TypeScript with YAML](https://github.com/microsoft/TypeScript/wiki/YAML)
- [YAML 공식 문서](https://yaml.org/)
- [YAML 파싱 라이브러리](https://www.npmjs.com/package/yaml)
- [YAML을 JSON으로 변환하는 방법](https://www.tutorialspoint.com/how-to-convert-yaml-file-to-json-in-node-js)
- [JSON을 YAML로 변환하는 방법](https://www.npmjs.com/package/yamlify)