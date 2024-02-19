---
aliases:
- /ko/typescript/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:12.661125-07:00
description: "YAML\uC740 \uC0AC\uB78C \uCE5C\uD654\uC801\uC73C\uB85C \uC124\uACC4\uB41C\
  \ \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uB85C, \uC124\uC815 \uD30C\uC77C\
  , \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\uC9D5 \uBC0F \uB370\uC774\uD130 \uC800\
  \uC7A5\uC5D0 \uC790\uC8FC \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uBCF5\uC7A1\uD55C \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\
  \uB97C \uB2E4\uB8F0 \uB54C YAML\uC758 \uAC00\uB3C5\uC131\uACFC \uC0AC\uC6A9\uC758\
  \ \uC6A9\uC774\uC131 \uB54C\uBB38\uC5D0 YAML\uC5D0 \uC758\uC874\uD558\uB294 \uACBD\
  \uD5A5\uC774 \uC788\uC73C\uBA70, \uC774\uB294 TypeScript\uB85C\u2026"
lastmod: 2024-02-18 23:09:05.850910
model: gpt-4-0125-preview
summary: "YAML\uC740 \uC0AC\uB78C \uCE5C\uD654\uC801\uC73C\uB85C \uC124\uACC4\uB41C\
  \ \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uB85C, \uC124\uC815 \uD30C\uC77C\
  , \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\uC9D5 \uBC0F \uB370\uC774\uD130 \uC800\
  \uC7A5\uC5D0 \uC790\uC8FC \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uBCF5\uC7A1\uD55C \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\
  \uB97C \uB2E4\uB8F0 \uB54C YAML\uC758 \uAC00\uB3C5\uC131\uACFC \uC0AC\uC6A9\uC758\
  \ \uC6A9\uC774\uC131 \uB54C\uBB38\uC5D0 YAML\uC5D0 \uC758\uC874\uD558\uB294 \uACBD\
  \uD5A5\uC774 \uC788\uC73C\uBA70, \uC774\uB294 TypeScript\uB85C\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
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
