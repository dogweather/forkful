---
title:                "yaml로 작업하기"
html_title:           "PowerShell: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇인가요?

YAML을 사용하는 것은 무엇일까요? 프로그래머들이 YAML을 사용하는 이유는 무엇일까요?

YAML은 파일 포맷의 일종으로, 데이터를 관리하고 저장할 때 사용됩니다. 이를 통해 여러분은 구조화된 데이터를 읽고 쓰기가 더욱 쉽고 명확하게 됩니다. 그리고 YAML은 소프트웨어의 구성 파일로도 사용됩니다. 따라서 YAML을 사용하면 여러분의 코드를 더욱 유지보수하기 쉽고 효율적으로 만들 수 있습니다.

## 어떻게 하나요?

여러분은 PowerShell을 사용하여 YAML 파일을 읽고 쓸 수 있습니다.

```PowerShell
# YAML 파일 읽기
$yamlData = Get-Content -Path "example.yaml" | ConvertFrom-Yaml

# YAML 파일 쓰기
$newData = @{
    myInfo = @{
        name = "John"
        age = 25
    }
}
$newData | ConvertTo-Yaml | Out-File -Path "new.yaml"
```

`ConvertFrom-Yaml`을 사용하면 YAML 파일을 PowerShell 객체로 변환할 수 있습니다. 또한 `ConvertTo-Yaml`을 사용하면 PowerShell 객체를 YAML 형식의 문자열로 변환할 수 있습니다.

## 더 깊게 들어가보기

YAML은 프로그래밍 언어가 아닌 데이터 직렬화 언어입니다. 주로 인간이 읽고 쓰기 쉽도록 디자인되어 있습니다. YAML은 XML과 JSON과 같은 다른 데이터 형식과 비교하여 가독성이 더 좋습니다.

YAML은 간략한 구문과 들여쓰기에 의존하여 데이터를 구조화합니다. 이러한 간결성은 YAML을 사용하여 복잡한 데이터 구조를 일관적이고 명확하게 표현하는 데 도움이 됩니다.

YAML은 대화형 설정 파일에서부터 대규모 응용 프로그램에서까지 다양한 용도로 사용될 수 있습니다. 또한 다른 데이터 형식과 호환되는 여러 라이브러리를 지원하므로 다른 언어에서도 YAML을 사용할 수 있습니다.

## 관련 링크

- [YAML 문서](https://yaml.org/spec/)
- [PowerShell 문서](https://docs.microsoft.com/en-us/powershell/scripting/overview)