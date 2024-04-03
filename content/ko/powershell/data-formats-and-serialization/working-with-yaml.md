---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:15.887274-07:00
description: "YAML, \uB610\uB294 YAML Ain't Markup Language\uB294 \uC0AC\uB78C\uC774\
  \ \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC8FC\uB85C \uC124\
  \uC815 \uD30C\uC77C\uACFC \uC5B8\uC5B4\uAC04\uC758 \uB370\uC774\uD130 \uC804\uC1A1\
  \uC744 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uADF8\uAC83\uC758 \uB2E8\uC21C\
  \uC131\uACFC \uAC00\uB3C5\uC131\uC740 \uD658\uACBD, \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158, \uB610\uB294 \uC124\uC815\uC774 \uC911\uC694\uD558\uACE0 \uC27D\uAC8C \uC774\
  \uD574\uB418\uACE0 \uD3B8\uC9D1\uB420\u2026"
lastmod: '2024-03-13T22:44:55.576259-06:00'
model: gpt-4-0125-preview
summary: "YAML, \uB610\uB294 YAML Ain't Markup Language\uB294 \uC0AC\uB78C\uC774 \uC77D\
  \uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uC785\
  \uB2C8\uB2E4."
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
