---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:45.226306-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC904\uC784\uB9D0\uB85C\
  , \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uADF8\uAC83\uC758 \uB2E8\uC21C\uC131\uACFC \uC77D\uAE30 \uC26C\uC6C0 \uB54C\uBB38\
  \uC5D0, \uC8FC\uB85C \uAD6C\uC131 \uD30C\uC77C \uBC0F \uC5B8\uC5B4 \uAC04 \uB370\
  \uC774\uD130 \uAD50\uD658\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\uB294 JSON\uC774\
  \uB098 XML\uC5D0 \uBE44\uD574 \uC6B0\uC218\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.820617-06:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC904\uC784\uB9D0\uB85C\
  , \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uADF8\uAC83\uC758 \uB2E8\uC21C\uC131\uACFC \uC77D\uAE30 \uC26C\uC6C0 \uB54C\uBB38\
  \uC5D0, \uC8FC\uB85C \uAD6C\uC131 \uD30C\uC77C \uBC0F \uC5B8\uC5B4 \uAC04 \uB370\
  \uC774\uD130 \uAD50\uD658\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\uB294 JSON\uC774\
  \uB098 XML\uC5D0 \uBE44\uD574 \uC6B0\uC218\uD569\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 "YAML Ain't Markup Language"의 줄임말로, 사람이 읽을 수 있는 데이터 직렬화 포맷입니다. 프로그래머들은 그것의 단순성과 읽기 쉬움 때문에, 주로 구성 파일 및 언어 간 데이터 교환에 사용합니다. 이는 JSON이나 XML에 비해 우수합니다.

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
