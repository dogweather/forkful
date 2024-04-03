---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:45.226306-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694? \uC790\uBC14\uC2A4\uD06C\uB9BD\
  \uD2B8\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uB300\uCCB4\uB85C\
  \ \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uAC8C \uB429\
  \uB2C8\uB2E4. \uC774\uB294 \uC5B8\uC5B4 \uC790\uCCB4\uC5D0 YAML \uD30C\uC11C\uAC00\
  \ \uB0B4\uC7A5\uB418\uC5B4 \uC788\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC785\uB2C8\uB2E4\
  . \uC774 \uBAA9\uC801\uC744 \uC704\uD574 \uAC00\uC7A5 \uC778\uAE30 \uC788\uB294\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC911 \uD558\uB098\uB294 `js-yaml`\uC785\uB2C8\
  \uB2E4. `js-yaml`\uC744 \uC0AC\uC6A9\uD558\uC5EC\u2026"
lastmod: '2024-03-13T22:44:55.820617-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C YAML\uC744 \uB2E4\uB8E8\
  \uB294 \uAC83\uC740 \uB300\uCCB4\uB85C \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD558\uAC8C \uB429\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 어떻게 하나요?
자바스크립트에서 YAML을 다루는 것은 대체로 제3자 라이브러리를 사용하게 됩니다. 이는 언어 자체에 YAML 파서가 내장되어 있지 않기 때문입니다. 이 목적을 위해 가장 인기 있는 라이브러리 중 하나는 `js-yaml`입니다. `js-yaml`을 사용하여 YAML을 자바스크립트 객체로 파싱하거나 그 반대의 작업을 수행할 수 있습니다.

먼저, `js-yaml`를 설치해야 합니다:

```bash
npm install js-yaml
```

그런 다음, 프로젝트에서 사용할 수 있습니다. 여기 YAML 파일을 로드하고 자바스크립트 객체로 파싱하는 방법이 있습니다:

```javascript
// js-yaml 모듈을 요구합니다
const yaml = require('js-yaml');
const fs   = require('fs');

// 파일에서 YAML 로드
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

만약 당신의 `config.yaml` 파일이 이렇게 생겼다면:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

결과물은 이렇게 됩니다:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

반대로, 자바스크립트 객체를 YAML 문자열로 변환하는 방법은:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

이 코드는 다음을 생성할 것입니다:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

`js-yaml`을 사용하면, JavaScript 프로젝트에 YAML 파싱 및 직렬화를 쉽게 통합하여, 데이터 상호 교환성 및 구성 관리를 향상시킬 수 있습니다.
