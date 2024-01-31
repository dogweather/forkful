---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
프로그래머들이 YAML을 다루는 것은 설정, 인프라 코드, 데이터 교환 등을 위해 사용되는 데이터 직렬화 형식입니다. YAML은 읽기 쉽고 간단해서 널리 채택되고 있어요.

## How to:
YAML 파일을 읽고 쓰는 예제를 확인해보세요.

```TypeScript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// YAML 파일 읽기
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.safeLoad(fileContents);
console.log(data);

// YAML 파일 쓰기
const newData = { title: 'YAML Example', value: 42 };
const yamlStr = yaml.safeDump(newData);
fs.writeFileSync('./new-config.yaml', yamlStr, 'utf8');
```

실행 결과는 `config.yaml`의 내용과 새로 작성된 `new-config.yaml` 파일 내용을 보여주게 되겠죠.

## Deep Dive
YAML은 "YAML Ain't Markup Language" (원래는 "Yet Another Markup Language")의 재귀 약어이며, XML이나 JSON처럼 데이터를 표현하기 위한 언어예요. JSON에 비해 가독성이 뛰어나지만, 파싱하기는 조금 더 복잡해요. `js-yaml` 라이브러리는 TypeScript나 JavaScript에서 YAML을 쉽게 다룰 수 있게 해줍니다. 대안으로는 `yamljs`, `yaml` 등이 있어요.

## See Also
- YAML 공식 사이트: https://yaml.org
- `js-yaml` GitHub 페이지: https://github.com/nodeca/js-yaml
- TypeScript 공식 페이지: https://www.typescriptlang.org 

온라인 YAML 검증 도구를 사용하면 데이터 구조가 올바른 YAML인지 확인할 수 있어요.
