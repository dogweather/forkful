---
title:                "yaml 작업하기"
html_title:           "TypeScript: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

여러분은 아마도 프로그래밍 분야에서 YAML을 사용하거나 들어보았을 것입니다. YAML은 데이터 직렬화 언어로, 구문이 간단하고 읽고 쓰기 쉽기 때문에 데이터 구조를 관리할 때 자주 사용됩니다. 이 글에서는 TypeScript에서 YAML을 어떻게 다루는지 알아보겠습니다.

## 방법

먼저, `yaml` 패키지를 설치해야 합니다. `npm install yaml` 명령어를 사용하면 됩니다. 그리고 프로그램에서 다음과 같은 코드로 YAML 파일을 읽고 쓸 수 있습니다.

```TypeScript
import * as yaml from 'yaml';

// YAML 파일을 읽어오기
const data = yaml.parse(fs.readFileSync('config.yaml', 'utf8'));

// YAML 파일에 쓰기
fs.writeFileSync('new-config.yaml', yaml.stringify(data));
```

위의 코드에서 `yaml.parse()` 함수는 YAML 파일을 읽어서 JavaScript 객체로 변환해줍니다. 이렇게 변환된 객체를 프로그램에서 필요한 대로 다룰 수 있습니다. 마찬가지로 `yaml.stringify()` 함수는 JavaScript 객체를 YAML 형식의 문자열로 변환하여 파일에 쓸 수 있도록 해줍니다.

만약 YAML 파일을 예쁘게 정렬하여 출력하고 싶다면, `yaml.stringify()` 함수에 두 번째 인자로 `{ indentSeq: false }`를 넣어주면 됩니다. 아래 예시를 참고하세요.

```TypeScript
const data = {
  name: 'Jane',
  age: 26,
  favoriteFoods: ['pizza', 'ice cream', 'sushi']
};

console.log(yaml.stringify(data, { indentSeq: false }));

// 출력 결과:
// name: Jane
// age: 26
// favoriteFoods:
//   - pizza
//   - ice cream
//   - sushi
```

## 깊이 있는 설명

YAML을 사용하면 데이터를 계층적으로 표현할 수 있습니다. 이는 여러분이 복잡한 데이터 구조를 관리할 때 유용합니다. 하지만 주의할 점이 있습니다. YAML에서는 들여쓰기로 계층을 표현하기 때문에, 들여쓰기 규칙을 잘 지켜야 합니다. 들여쓰기에 공백(4칸)과 탭(\t)을 혼용해서 사용하는 것은 좋지 않으며, 같은 파일에서도 일관성을 지켜야 합니다.

그리고 YAML에서는 배열을 [] 대신 - 기호를 사용하여 나타냅니다. 예를 들어, `favoriteFoods`에 pizza, ice cream, sushi가 배열로 들어있는 것을 볼 수 있습니다.

만약 YAML 파일을 내보내거나 읽는 데 문제가 발생하면, 문법에 오류가 있는지 확인해보세요. YAML 문법은 간단하지만, 잘못된 들어쓰기나 부호를 사용하면 제대로 읽거나 쓸 수 없습니다.

## 참고

- [YAML 사용 예시 (TypeScript)](https://github.com/eemeli/yaml/blob/master/examples/typescript.ts)
- [YAML 문법](https://yaml.org/spec/1.2/spec.html)
- [YAML 패키지 문서](https://www.npmjs.com/package/yaml)