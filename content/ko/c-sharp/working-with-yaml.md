---
title:                "yaml 작업하기"
html_title:           "C#: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML은 여러 플랫폼에서 데이터를 구성하고 전송하는 데에 사용됩니다. 이는 읽기 쉽고 명확한 구문을 가지고 있으며, 소프트웨어 개발에서 중요한 역할을 합니다.

## 어떻게

```C#
using YamlDotNet.Serialization;
using System;
using System.IO;

// YAML 파일 읽기
var input = new StringReader("---\nstring: YAML 플레인 텍스트\n");
var deserializer = new Deserializer();
var yamlObject = deserializer.Deserialize(input);

// YAML 데이터 출력
Console.WriteLine(yamlObject);

// YAML 파일 쓰기
var serializer = new Serializer();
var yaml = serializer.Serialize(yamlObject);

// YAML 파일 인코딩
var encoding = new UTF8Encoding(false);
var bytes = encoding.GetBytes(yaml);

// YAML 파일 저장
using (var file = File.OpenWrite("sample.yaml"))
{
  file.Write(bytes, 0, bytes.Length);
}

// 예제 출력
// "{ string = \"YAML 플레인 텍스트\" }"
```

## 더 깊게 들어가기

YAML에서 사용할 수 있는 여러 타입과 데이터 구조에 대해 배우고, 시퀀스(sequence)와 매핑(mapping) 등의 YAML의 기능에 대해 알아보세요. 또한 YAML 파일의 주석(Comments)을 사용하여 코드를 문서화하는 방법도 익힐 수 있습니다.

## 참고

- [YAML 공식 사이트](https://yaml.org/)
- [YAML 사용 예시](https://github.com/dotnet/corefx/tree/master/src/System.ComponentModel.Primitives/tests/Yaml)
- [YamlDotNet 패키지 공식 문서](https://github.com/aaubry/YamlDotNet/wiki)