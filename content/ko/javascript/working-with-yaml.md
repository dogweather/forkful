---
title:                "Javascript: Yaml 작업하기"
simple_title:         "Yaml 작업하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜?

YAML은 자바스크립트 프로그래밍에서 사용할 수 있는 강력한 데이터 시리얼라이제이션 언어입니다. 이 언어를 사용하면 코드의 가독성을 높이고 더 적은 라인을 사용하여 데이터를 표현할 수 있습니다. 또한 개발 과정에서 설정 파일을 쉽게 관리하고 조작할 수 있으며, 여러 언어에서 사용 가능하기 때문에 유연하게 작업할 수 있습니다.

## 어떻게 해야 할까요?

```javascript
// YAML 데이터 생성
const yamlData = `
fruits:
- apple
- banana
- orange
`;

// YAML 파싱
const parsedYaml = YAML.parse(yamlData);
console.log(parsedYaml.fruits); // ['apple', 'banana', 'orange']

// YAML 스트림 파싱
const yamlStreamData = `
fruits:
  - apple
  - banana
  - orange
`;
YAML.parseStream(yamlStreamData, function (data) {
  console.log(data); // { fruits: ['apple', 'banana', 'orange'] }
});
```

위 예시 코드에서는 YAML 데이터를 생성하고, `YAML.parse()`를 사용하여 파싱한 뒤 원하는 데이터를 가져오는 방법을 보여줍니다. 또한 `YAML.parseStream()`을 사용하면 YAML 스트림을 파싱할 수도 있습니다. 자세한 설명과 다양한 예시는 아래의 "더 깊이 파헤치기" 섹션에서 확인할 수 있습니다.

## 더 깊이 파헤치기

YAML은 키-값 쌍을 사용하여 데이터를 표현하는 것이 가장 기본적인 형식이며, 들여쓰기로 중첩 데이터를 구분합니다. 또한 배열과 객체를 사용하여 다양한 형태의 데이터를 표현할 수 있습니다. YAML은 주석을 사용하여 코드를 설명하거나 임시로 주석 처리할 수도 있습니다. 또한 여러 프로그래밍 언어에 쉽게 포팅될 수 있기 때문에 유용하게 사용할 수 있는 언어입니다.

## See Also

- [YAML 공식 문서](https://yaml.org/)
- [YAML 사용 예시 블로그 포스트](https://medium.com/swlh/yaml-tutorial-js-yaml-for-your-nodejs-project-1068d6368167)
- [YAML을 사용한 React 프로젝트 설정 파일 예시](https://medium.com/better-programming/how-to-set-up-a-react-app-with-yaml-for-configs-9c304bd4b957)