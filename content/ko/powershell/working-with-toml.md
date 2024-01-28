---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:25:33.388975-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

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
