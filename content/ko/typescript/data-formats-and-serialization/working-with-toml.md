---
date: 2024-01-26 04:27:11.893194-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C\
  \ JSON\uC774\uB098 YAML\uACFC \uAC19\uC740 \uB370\uC774\uD130 \uC9C1\uB82C\uD654\
  \ \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uADF8\
  \uAC83\uC758 \uC778\uAC04\uC774 \uC77D\uAE30 \uC26C\uC6C0\uACFC \uB370\uC774\uD130\
  \ \uD0C0\uC785\uC73C\uB85C\uC758 \uC9C1\uAD00\uC801\uC778 \uB9E4\uD551 \uB54C\uBB38\
  \uC5D0 \uC124\uC815 \uD30C\uC77C\uACFC \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\
  \uD55C \uC8FC\uC694 \uC120\uD0DD\uC9C0\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.896706-07:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C JSON\uC774\
  \uB098 YAML\uACFC \uAC19\uC740 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD3EC\uB9F7\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uADF8\uAC83\uC758\
  \ \uC778\uAC04\uC774 \uC77D\uAE30 \uC26C\uC6C0\uACFC \uB370\uC774\uD130 \uD0C0\uC785\
  \uC73C\uB85C\uC758 \uC9C1\uAD00\uC801\uC778 \uB9E4\uD551 \uB54C\uBB38\uC5D0 \uC124\
  \uC815 \uD30C\uC77C\uACFC \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD55C \uC8FC\
  \uC694 \uC120\uD0DD\uC9C0\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 약자로 JSON이나 YAML과 같은 데이터 직렬화 포맷입니다. 프로그래머들은 그것의 인간이 읽기 쉬움과 데이터 타입으로의 직관적인 매핑 때문에 설정 파일과 데이터 교환을 위한 주요 선택지로 사용합니다.

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
