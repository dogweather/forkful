---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:15.887274-07:00
description: "\uBC29\uBC95: \uAE30\uBCF8\uC801\uC73C\uB85C PowerShell\uC740 YAML\uC744\
  \ \uAD6C\uBB38 \uBD84\uC11D\uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5\uB41C cmdlet\uC744\
  \ \uC81C\uACF5\uD558\uC9C0 \uC54A\uC9C0\uB9CC, `powershell-yaml` \uBAA8\uB4C8\uC744\
  \ \uD65C\uC6A9\uD558\uAC70\uB098 `yq`\uC640 \uAC19\uC740 \uB3C4\uAD6C\uC640 \uD568\
  \uAED8 `ConvertFrom-Json`\uC744 \uC0AC\uC6A9\uD558\uC5EC YAML\uC744 PowerShell \uAC1D\
  \uCCB4\uB85C \uBCC0\uD658\uD560\u2026"
lastmod: '2024-03-13T22:44:55.576259-06:00'
model: gpt-4-0125-preview
summary: "\uAE30\uBCF8\uC801\uC73C\uB85C PowerShell\uC740 YAML\uC744 \uAD6C\uBB38\
  \ \uBD84\uC11D\uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5\uB41C cmdlet\uC744 \uC81C\uACF5\
  \uD558\uC9C0 \uC54A\uC9C0\uB9CC, `powershell-yaml` \uBAA8\uB4C8\uC744 \uD65C\uC6A9\
  \uD558\uAC70\uB098 `yq`\uC640 \uAC19\uC740 \uB3C4\uAD6C\uC640 \uD568\uAED8 `ConvertFrom-Json`\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC YAML\uC744 PowerShell \uAC1D\uCCB4\uB85C \uBCC0\uD658\
  \uD560 \uB54C YAML\uACFC \uC6D0\uD65C\uD558\uAC8C \uC791\uB3D9\uD569\uB2C8\uB2E4\
  ."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

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
