---
date: 2024-01-26 04:25:33.388975-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C\
  , \uBD84\uBA85\uD55C \uC758\uBBF8 \uCCB4\uACC4\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\
  \uC6B4 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC778\uAC04\uC774 \uC77D\uAE30 \uC27D\uACE0\
  \ \uAE30\uACC4\uC640 \uCE5C\uD654\uC801\uC778 \uADE0\uD615\uC744 \uB9DE\uCD98\uB2E4\
  \uB294 \uC774\uC810 \uB54C\uBB38\uC5D0 \uAD6C\uC131 \uD30C\uC77C\uB85C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.581108-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C, \uBD84\
  \uBA85\uD55C \uC758\uBBF8 \uCCB4\uACC4\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4\
  \ \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC778\uAC04\uC774 \uC77D\uAE30 \uC27D\uACE0 \uAE30\
  \uACC4\uC640 \uCE5C\uD654\uC801\uC778 \uADE0\uD615\uC744 \uB9DE\uCD98\uB2E4\uB294\
  \ \uC774\uC810 \uB54C\uBB38\uC5D0 \uAD6C\uC131 \uD30C\uC77C\uB85C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 무엇 & 왜?

TOML은 Tom's Obvious, Minimal Language의 약자로, 분명한 의미 체계로 인해 읽기 쉬운 데이터 직렬화 형식입니다. 프로그래머들은 인간이 읽기 쉽고 기계와 친화적인 균형을 맞춘다는 이점 때문에 구성 파일로 사용합니다.

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
