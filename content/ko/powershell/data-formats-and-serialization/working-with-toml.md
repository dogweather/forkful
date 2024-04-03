---
date: 2024-01-26 04:25:33.388975-07:00
description: "\uBC29\uBC95: PowerShell\uC5D0\uC11C TOML\uC744 \uAD6C\uBB38 \uBD84\uC11D\
  \uD558\uB294 \uAE30\uBCF8 cmdlet\uC740 \uC5C6\uC2B5\uB2C8\uB2E4. PowerShell\uB85C\
  \ \uC791\uC5C5\uD558\uB824\uBA74 `toml-to-json`\uACFC \uAC19\uC740 \uB3C4\uAD6C\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC TOML\uC744 JSON\uC73C\uB85C \uBCC0\uD658\uD558\uAC70\uB098\
  \ \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\uC801\uC785\
  \uB2C8\uB2E4. \uAC00\uC0C1 \uBAA8\uB4C8\u2026"
lastmod: '2024-03-13T22:44:55.581108-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C TOML\uC744 \uAD6C\uBB38 \uBD84\uC11D\uD558\uB294\
  \ \uAE30\uBCF8 cmdlet\uC740 \uC5C6\uC2B5\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
PowerShell에서 TOML을 구문 분석하는 기본 cmdlet은 없습니다. PowerShell로 작업하려면 `toml-to-json`과 같은 도구를 사용하여 TOML을 JSON으로 변환하거나 모듈을 사용하는 것이 일반적입니다. 가상 모듈 `PowerShellTOML`을 사용한 방법은 다음과 같습니다:

```PowerShell
# 먼저 모듈을 설치합니다(상상의 모듈, 시연용)
Install-Module PowerShellTOML

# TOML 파일을 가져옵니다
$config = Import-TomlConfig -Path './config.toml'

# 값을 접근하는 방법
Write-Output $config.database.server

# 'config.toml'의 샘플 TOML 내용:
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# 샘플 출력:
# 192.168.1.1
```

## 심층 탐구
TOML은 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 구성 파일을 위한 XML과 YAML에 비해 더 간단한 대안으로 창조되었습니다. 그 첫 번째 버전은 2013년에 등장했습니다. TOML은 JSON과 비교될 수 있지만, 사람이 유지 관리하는 구성에 적합한 선택으로 만들기 위해 더 인간친화적으로 설계되었습니다. 대안은 YAML, JSON 및 XML을 포함합니다.

구현 측면에서, TOML용 PowerShell 모듈은 일반적으로 C#과 같은 보다 성능 지향적인 언어로 작성된 TOML 라이브러리를 둘러싼 래퍼일 것입니다. PowerShell에는 TOML에 대한 내장 지원이 없기 때문에, 이러한 모듈이 TOML 형식과 편리하게 인터페이스하는 데 필요합니다.

## 참조
- TOML 표준: https://toml.io/en/
- `toml` PowerShell 모듈의 GitHub 저장소(읽는 시점에 존재하는 경우): https://github.com/powershell/PowerShellTOML
- TOML 소개: https://github.com/toml-lang/toml
- 데이터 직렬화 형식 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
