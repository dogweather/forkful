---
title:                "프로그래머를 위한 TOML 다루기"
aliases: - /ko/c-sharp/working-with-toml.md
date:                  2024-01-26T04:20:53.488213-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 톰의 명백하고 최소한의 언어(Tom's Obvious, Minimal Language)를 위한 머릿글자로, 명확한 의미론 덕분에 쉽게 읽히는 설정 파일 형식입니다. 프로그래머들은 시스템간의 데이터 교환을 단순화하고 인간의 가독성과 기계의 구문 분석 가능성 사이의 균형을 이루기 때문에 이를 설정 파일용으로 사용합니다.

## 방법:
먼저, `Tomlyn` 같은 TOML 파서를 설치하세요. 패키지 관리자를 사용합니다:

```csharp
dotnet add package Tomlyn
```

다음으로, TOML 파일을 구문 분석합니다:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"소유자: {tomlTable["owner"]["name"]}");
// 출력:
// 소유자: Tom Preston-Werner
```

이제, TOML을 생성하고 작성합니다:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("config.toml에 TOML 작성됨");
// 출력:
// config.toml에 TOML 작성됨
```

## 심층 탐색:
TOML은 구성 설정에서 YAML과 JSON과 같은 기존 형식의 한계에 대한 반응으로 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 2013년경에 만들어졌습니다. 이는 명확하고 모호하지 않은 것에 중점을 둔 구성을 위해 특별히 설계되었습니다.

다른 구성 형식으로는 YAML, JSON, 그리고 XML이 있습니다. 그러나 TOML은 특히 수동으로 편집하는 구성 파일에서 더 인간 친화적이라는 점에서 두드러집니다. JSON은 흔하긴 하지만 복잡한 구성의 경우 가독성이 떨어지고, XML은 장황합니다. YAML도 가독성 면에서 비슷하지만, 공백 사용이 많고 특정 내용에 대한 보안 위험이 있어 복잡해질 수 있습니다.

구현 측면에서 TOML은 해시 테이블에 깨끗하게 매핑되는 데 중점을 둬 데이터 추출이 예측 가능하도록 합니다. 1.0.0 버전이 출시되면서 TOML은 그 명세를 공고히 하고 안정성 및 도구 지원을 개선했습니다.

## 참고:
- 공식 TOML GitHub 저장소 & 명세: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- .NET 라이브러리 Tomlyn: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
