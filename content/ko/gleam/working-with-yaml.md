---
title:                "Gleam: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜?

Gleam을 사용하면 YAML 파일을 쉽게 작성하고 읽을 수 있습니다. YAML은 데이터를 인간이 읽을 수 있는 형태로 저장하기에 적합하며, Gleam은 이를 처리하는 데 최적화되어 있습니다.

## 어떻게?

Gleam에서 YAML을 작성하는 방법은 간단합니다. 먼저 `gleam_yml` 패키지를 임포트한 다음, `Yaml.Writer` 모듈에서 데이터를 작성할 수 있습니다.

```
import gleam_yml
import gleam_yml.Yaml.Writer

let data = 
  [
    { id: 1, name: "John Doe" },
    { id: 2, name: "Jane Smith" }
  ]

let yaml = 
  data
  |> Yaml.Writer.list(
    \rec(person) ->
      person
      |> Yaml.Writer.map([
        Yaml.Writer.field("id", Yaml.Writer.int),
        Yaml.Writer.field("name", Yaml.Writer.string)
      ])
  )
  |> Yaml.Writer.encode

```

위의 코드를 실행하면 다음과 같은 YAML 파일이 생성됩니다.

```
- id: 1
  name: "John Doe"
- id: 2
  name: "Jane Smith"
```

객체나 리스트를 YAML로 작성하는 데 사용할 수 있는 다양한 함수가 있습니다. 자세한 정보는 [공식 문서](https://gleam.run/packages/gleam_yml/latest/gleam_yml/Yaml.Writer.html)에서 확인할 수 있습니다.

## 깊이있게 알아보기

Gleam의 `gleam_yml` 패키지는 YAML을 작성하는 것뿐만 아니라 읽는 데에도 사용할 수 있습니다. `Yaml.Reader` 모듈에서 YAML을 파싱해 데이터로 변환할 수 있습니다.

```
import gleam_yml
import gleam_yml.Yaml.Reader

let yaml = "
---
- id: 1
  name: 'John Doe'
- id: 2
  name: 'Jane Smith'
"

let data = yaml
  |> Yaml.Reader.decode
```

`data` 변수에는 다음과 같은 데이터가 저장됩니다.

```
[
  { id: 1, name: "John Doe" },
  { id: 2, name: "Jane Smith" }
]
```

또한 `Yaml.Reader` 모듈에서는 YAML을 다양한 타입으로 변환하는 함수도 제공합니다. 자세한 내용은 [공식 문서](https://gleam.run/packages/gleam_yml/latest/gleam_yml/Yaml.Reader.html)에서 확인할 수 있습니다.

## 참고

- [YAML 공식 사이트](https://yaml.org/)
- [Gleam YAML 패키지 공식 문서](https://gleam.run/packages/gleam_yml/latest/gleam_yml/Yaml.html)