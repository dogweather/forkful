---
date: 2024-01-26 04:27:11.893194-07:00
description: "\uBC29\uBC95: \uBA3C\uC800, TOML \uD30C\uC11C\uAC00 \uD544\uC694\uD569\
  \uB2C8\uB2E4. `@iarna/toml`\uC740 \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC785\uB2C8\
  \uB2E4. npm\uC744 \uD1B5\uD574 \uC124\uCE58\uD558\uC138\uC694: `npm install @iarna/toml\
  \ --save`. \uB2E4\uC74C\uC740 TOML \uD30C\uC77C\uC744 \uC77D\uACE0 JavaScript \uAC1D\
  \uCCB4\uB85C \uD30C\uC2F1\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.685860-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
먼저, TOML 파서가 필요합니다. `@iarna/toml`은 인기 있는 선택입니다. npm을 통해 설치하세요: `npm install @iarna/toml --save`. 다음은 TOML 파일을 읽고 JavaScript 객체로 파싱하는 방법입니다:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
만약 `config.toml`이 다음을 포함한다면:
```
[server]
port = 8080
```
출력될 것은:
```
{ server: { port: 8080 } }
```
그리고, TOML 파일에 쓰는 것도 마찬가지로 간단합니다:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
이 코드를 실행하면 `config.toml`에 객체를 TOML 형식으로 쓰게 됩니다.

## 심층 탐구
TOML은 GitHub의 공동 창립자인 Tom Preston-Werner가 2013년쯤 INI나 YAML과 같은 다른 형식의 한계를 인식하여 반응해 만들었습니다. 이는 모호함 없이 데이터 구조로 쉽게 파싱되도록 설계되었고, 그래서 설정 파일을 위한 선호도가 높습니다. JSON은 주석이 없는 반면 YAML은 더 복잡합니다. TOML은 간단함과 복잡한 데이터 계층을 명확하게 표현할 수 있는 능력에서 빛납니다.

내부적으로, TypeScript에서 TOML을 파싱할 때, 당신은 텍스트 데이터를 언어가 조작할 수 있는 구조화된 형식으로 변환하고 있습니다. 이것은 토큰으로 원시 텍스트를 변환하는 렉싱(lexing)과 내부 데이터 구조를 구축하는 파싱(parsing)을 포함합니다; `@iarna/toml`은 이 둘을 모두 원활하게 처리합니다. 이모지 지원은 TOML의 사용자 중심 접근 방식을 보여주는 재미있는 터치입니다.

## 참고
- TOML 공식 스펙: https://toml.io/en/
- `@iarna/toml` 패키지: https://www.npmjs.com/package/@iarna/toml
- TOML, YAML, 그리고 JSON 사이의 비교: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
