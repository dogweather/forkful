---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:12.661125-07:00
description: "\uBC29\uBC95: TypeScript\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\uB294 \uAC83\
  \uC740 \uC77C\uBC18\uC801\uC73C\uB85C YAML \uB0B4\uC6A9\uC744 JavaScript \uAC1D\uCCB4\
  \uB85C \uAD6C\uBB38 \uBD84\uC11D\uD558\uACE0, \uAC00\uB2A5\uD558\uB2E4\uBA74 JavaScript\
  \ \uAC1D\uCCB4\uB97C \uB2E4\uC2DC YAML\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB97C \uC704\uD574\uC11C\uB294 \uAD6C\uBB38\
  \ \uBD84\uC11D\uAE30\uAC00 \uD544\uC694\uD55C\uB370, \uC778\uAE30 \uC788\uB294 \uC120\
  \uD0DD \uC911 \uD558\uB098\uB294\u2026"
lastmod: '2024-03-13T22:44:54.882604-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uC77C\
  \uBC18\uC801\uC73C\uB85C YAML \uB0B4\uC6A9\uC744 JavaScript \uAC1D\uCCB4\uB85C \uAD6C\
  \uBB38 \uBD84\uC11D\uD558\uACE0, \uAC00\uB2A5\uD558\uB2E4\uBA74 JavaScript \uAC1D\
  \uCCB4\uB97C \uB2E4\uC2DC YAML\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

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
