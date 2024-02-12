---
title:                "YAML로 작업하기"
aliases:
- /ko/typescript/working-with-yaml.md
date:                  2024-02-03T19:27:12.661125-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML은 사람 친화적으로 설계된 데이터 직렬화 언어로, 설정 파일, 프로세스 간 메시징 및 데이터 저장에 자주 사용됩니다. 프로그래머들은 복잡한 구조화된 데이터를 다룰 때 YAML의 가독성과 사용의 용이성 때문에 YAML에 의존하는 경향이 있으며, 이는 TypeScript로 개발된 애플리케이션에 탁월한 선택이 됩니다.

## 방법:
TypeScript에서 YAML을 다루는 것은 일반적으로 YAML 내용을 JavaScript 객체로 구문 분석하고, 가능하다면 JavaScript 객체를 다시 YAML로 변환하는 것을 포함합니다. 이를 위해서는 구문 분석기가 필요한데, 인기 있는 선택 중 하나는 `js-yaml`로, TypeScript 프로젝트에 쉽게 통합될 수 있는 라이브러리입니다.

### js-yaml 설치하기
먼저, 프로젝트에 `js-yaml`을 추가하세요:

```bash
npm install js-yaml
```

### YAML을 JavaScript 객체로 구문 분석하기
다음과 같은 내용의 YAML 파일 `config.yaml`이 있다고 가정해 보세요:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

다음과 같이 이 파일을 읽고 JavaScript 객체로 구문 분석할 수 있습니다:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// YAML 파일을 읽고 구문 분석하기
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**출력 예시:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScript 객체를 YAML로 변환하기
반대로 JavaScript 객체를 YAML 문자열로 변환해야 하는 경우, 다음과 같이 `js-yaml`을 사용할 수 있습니다:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**출력 예시:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

이 스니펫은 JavaScript 객체를 YAML 문자열로 변환하여 출력합니다. 실제로는 이를 파일에 다시 쓰거나 애플리케이션의 다른 부분에서 사용할 수 있습니다.
