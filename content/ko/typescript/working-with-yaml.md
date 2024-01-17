---
title:                "YAML로 작업하기"
html_title:           "TypeScript: YAML로 작업하기"
simple_title:         "YAML로 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML 작업은 데이터 직렬화 및 구성 파일 생성을 위한 유용한 방법입니다. 프로그래머들은 YAML 작업을 통해 코드를 더 쉽게 읽고 유지 관리할 수 있으며, 가독성이 높은 구성 파일을 생성할 수 있습니다.

## 하고자하는 것:

```Typescript
// YAML 파일 읽기
const yaml = require('js-yaml');
const fs = require('fs');

try {
  // YAML 파일 로드
  const config = yaml.safeLoad(fs.readFileSync('config.yaml', 'utf8'));

  console.log(config);
} catch (error) {
  console.log(error);
}

// YAML 파일 생성
const yaml = require('js-yaml');
const fs = require('fs');

const config = {
  server: 'localhost',
  username: 'admin',
  password: '123456'
};

try {
  // YAML 파일 쓰기
  fs.writeFileSync('config.yaml', yaml.dump(config));
} catch (error) {
  console.log(error);
}
```

## 깊이 파고들기:

YAML은 2001년에 처음 나타난 마크업 언어인 XML의 대안으로 개발되었습니다. YAML은 코드의 가독성을 높이고, 파일 크기를 줄여주며, 사람이 읽고 수정하기 쉬운 방식으로 데이터를 표현합니다. 다른 대안으로는 JSON이 있지만, YAML은 그보다 사람이 읽고 작성하기 쉽기 때문에 프로그래머들 사이에서 인기가 높습니다. 타입스크립트에서 YAML을 작업하기 위해서는 'js-yaml' 라이브러리를 사용할 수 있습니다.

## 더 알아보기:

- YAML 공식 사이트: https://yaml.org/
- YAML 문서: https://yaml.org/spec/
- 타입스크립트에서 YAML 사용 예제: https://www.npmjs.com/package/js-yaml