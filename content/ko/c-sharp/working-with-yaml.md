---
title:                "C#: yaml과 함께 작업하기"
simple_title:         "yaml과 함께 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML을 사용해야 할까요?

YAML은 데이터를 구조화하고 효율적으로 관리하기 위해 사용되는 파일 형식입니다. C#을 이용해 YAML을 다룰 수 있다면, 프로그래밍 과정에서 더욱 용이하고 효율적인 방법으로 데이터를 다룰 수 있습니다.

## 사용 방법

YAML을 C#에서 사용하기 위해서는 먼저 YAML 라이브러리를 설치해야 합니다. 이후에는 아래 예시와 같이 YAML 파일을 읽고 쓰는 방법을 알아보겠습니다.

```C#
// YAML 라이브러리를 사용하기 위한 코드
using YamlDotNet.RepresentationModel;

// YAML 파일 경로 지정
string filePath = "example.yaml";

// YAML 파일 읽기
using (var reader = new StreamReader(filePath))
{
    // YAML 파일 내용을 문자열로 읽어오기
    string yamlString = reader.ReadToEnd();

    // YAML 문자열을 YAML 문서로 변환
    var yaml = new YamlStream();
    yaml.Load(new StringReader(yamlString));

    // YAML 문서에서 데이터 읽기
    var mapping = (YamlMappingNode)yaml.Documents[0].RootNode;
    string name = ((YamlScalarNode)mapping.Children[new YamlScalarNode("name")]).Value;

    // Output: John
    Console.WriteLine(name);
}

// YAML 파일 쓰기
using (var writer = new StreamWriter(filePath))
{
    // YAML 문서 생성
    var yaml = new YamlStream();

    // YAML 문서에 데이터 추가
    var mapping = new YamlMappingNode();
    mapping.Add("name", "Jane");

    // YAML 문서에 데이터 추가한 뒤 파일에 쓰기
    yaml.Add(mapping);
    yaml.Save(writer);
}
```

## 깊게 파보기

위 코드에서 보면, YAML 파일을 읽어오기 위해서는 `StreamReader` 클래스를, 쓰기 위해서는 `StreamWriter` 클래스를 사용해야 합니다. 또한 `YamlStream` 클래스를 이용해 YAML 문서를 생성하고, `YamlMappingNode` 클래스를 이용해 데이터를 다루게 됩니다. 추가적으로 `YamlDotNet.RepresentationModel` 네임스페이스에는 다양한 클래스와 메소드가 있으니, 더욱 깊게 공부하고 활용해보시기 바랍니다.

## 더 알아보기

- [YAML 공식 문서](https://yaml.org/)
- [YamlDotNet 라이브러리](https://github.com/aaubry/YamlDotNet)
- [C# YAML 사용법 포스팅](https://www.codeproject.com/articles/375166/yaml-parsing-with-csharp-using-yamldotnet)