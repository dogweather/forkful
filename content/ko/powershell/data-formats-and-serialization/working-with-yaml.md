---
title:                "YAML로 작업하기"
aliases:
- /ko/powershell/working-with-yaml/
date:                  2024-02-03T19:26:15.887274-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML, 또는 YAML Ain't Markup Language는 사람이 읽을 수 있는 데이터 직렬화 언어입니다. 프로그래머들은 주로 설정 파일과 언어간의 데이터 전송을 위해 사용합니다. 그것의 단순성과 가독성은 환경, 애플리케이션, 또는 설정이 중요하고 쉽게 이해되고 편집될 수 있어야 하는 작업에 특히 인기가 있습니다.

## 방법:
기본적으로 PowerShell은 YAML을 구문 분석하기 위한 내장된 cmdlet을 제공하지 않지만, `powershell-yaml` 모듈을 활용하거나 `yq`와 같은 도구와 함께 `ConvertFrom-Json`을 사용하여 YAML을 PowerShell 객체로 변환할 때 YAML과 원활하게 작동합니다.

### `powershell-yaml` 모듈 사용하기:
먼저 모듈을 설치하세요:
```PowerShell
Install-Module -Name powershell-yaml
```

YAML 파일을 읽으려면:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

PowerShell 객체를 YAML 파일로 쓰려면:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

`output.yml` 예시:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### `yq` 및 `ConvertFrom-Json`을 사용한 YAML 구문 분석:
다른 접근 방식은 `yq`, 경량이고 휴대 가능한 커맨드 라인 YAML 프로세서를 사용하는 것입니다. `yq`는 YAML을 JSON으로 변환할 수 있는데, 이는 PowerShell이 네이티브로 구문 분석할 수 있습니다.

먼저, 시스템에 `yq`가 설치되어 있는지 확인하세요.
그런 다음 실행하세요:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

이 방법은 크로스 플랫폼 환경에서 작업하거나 PowerShell 내에서 JSON을 선호하는 사용자에게 특히 유용합니다.
