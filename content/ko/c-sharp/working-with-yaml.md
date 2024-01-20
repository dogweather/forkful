---
title:                "yaml 사용하기"
html_title:           "C#: yaml 사용하기"
simple_title:         "yaml 사용하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML을 다루는 것은 사실 간단하지만 많은 혼란을 줄 수 있는 작업입니다. YAML은 데이터를 사람이 쉽게 읽고 작성할 수 있는 형식으로 나타내기 위해 만들어졌습니다. 프로그래머들은 YAML을 사용하여 복잡한 데이터 구조를 정의하고 관리하기 쉽게 만들 수 있습니다.

## 하우 투:
```C#
// YAML 파일 읽기
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.RepresentationModel;

var deserializer = new DeserializerBuilder().Build();
using (var reader = new StreamReader("file.yaml"))
{
    var yamlStream = new YamlStream();
    yamlStream.Load(reader);

    // 데이터 읽기
    var mapping = (YamlMappingNode)yamlStream.Documents[0].RootNode;
    var data = deserializer.Deserialize<Object>(mapping.Children);

    // 데이터 쓰기
    var serializer = new SerializerBuilder().Build();
    var writer = new StringWriter();
    serializer.Serialize(writer, data);
    Console.WriteLine(writer.ToString());
}

```

## 딥 다이브:
- YAML은 2001년에 개발되어 현재까지 많은 인기를 얻고 있습니다.
- YAML 외에도 XML, JSON 등 다양한 데이터 형식이 있지만, YAML은 구조적인 데이터를 다루는데 있어서는 가장 쉽고 명확한 형식입니다.
- YAML은 들여쓰기를 통해 데이터 구조를 정의하기 때문에 가독성이 매우 뛰어납니다. 이는 곧 다른 개발자들이 데이터를 이해하고 수정하기 쉽게 만들어 줍니다.

## 더 알아보기:
- [YAML 공식 사이트](https://yaml.org/)
- [YAML 와 JSON 비교](https://json2yaml.com/)