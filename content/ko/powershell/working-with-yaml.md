---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 데이터 직렬화 형식입니다. 프로그래머들은 설정, 배포 및 저장 정보와 같은 구조화된 데이터를 다루기 위해 YAML을 사용합니다.

## How to: (어떻게 하나요?)
PowerShell에서 YAML을 읽고 쓰려면, `powershell-yaml` 모듈을 사용하는 예제를 봅시다.

```PowerShell
# YAML 모듈 설치
Install-Module -Name powershell-yaml

# YAML 파일 읽기
$yamlContent = Get-Content -Path './example.yaml' | ConvertFrom-Yaml
$yamlContent

# YAML 데이터 생성 및 쓰기
$myData = @{
    name = 'Kim'
    role = 'Developer'
    languages = @('PowerShell', 'Python', 'C#')
}
$myData | ConvertTo-Yaml | Set-Content -Path './myData.yaml'

# 생성된 YAML 확인
Get-Content -Path './myData.yaml'
```
출력:
```yaml
name: Kim
role: Developer
languages:
  - PowerShell
  - Python
  - C#
```

## Deep Dive (깊이 파고들기)
YAML이 등장한 것은 2001년입니다. JSON과 XML 같은 다른 형식에 비해 가독성이 뛰어납니다. PowerShell에서는 `powershell-yaml`이나 `YamlDotNet` 같은 외부 모듈을 사용하여 YAML을 처리합니다. 내부적으로 라이브러리는 .NET 기반의 YamlDotNet을 활용하여 파싱과 직렬화를 수행합니다.

## See Also (더 보기)
- [powershell-yaml GitHub 페이지](https://github.com/cloudbase/powershell-yaml)
- [YamlDotNet GitHub 페이지](https://github.com/aaubry/YamlDotNet)
- [YAML 공식 웹사이트](https://yaml.org/)
