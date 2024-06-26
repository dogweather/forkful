---
date: 2024-01-26 03:37:55.537553-07:00
description: "\uBC29\uBC95: PowerShell\uC5D0\uB294 \uC804\uC6A9 \uB9AC\uD329\uD1A0\
  \uB9C1 \uB3C4\uAD6C\uAC00 \uB0B4\uC7A5\uB418\uC5B4 \uC788\uC9C0 \uC54A\uC9C0\uB9CC\
  , \uAC00\uB3C5\uC131\uACFC \uC131\uB2A5\uC744 \uC704\uD574 \uCF54\uB4DC\uB97C \uC815\
  \uB9AC\uD560 \uC218\uB294 \uC788\uC2B5\uB2C8\uB2E4. \uB108\uBB34 \uB9CE\uC740 \uC77C\
  \uC744 \uD558\uACE0 \uC788\uB294 \uD568\uC218\uB97C \uACE0\uB824\uD558\uACE0, \uC774\
  \uB97C \uBA85\uD655\uC131\uC744 \uC704\uD574 \uC5B4\uB5BB\uAC8C \uB9AC\uD329\uD1A0\
  \uB9C1\uD560 \uC218 \uC788\uB294\uC9C0 \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
lastmod: '2024-03-13T22:44:55.559314-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uB294 \uC804\uC6A9 \uB9AC\uD329\uD1A0\uB9C1 \uB3C4\uAD6C\
  \uAC00 \uB0B4\uC7A5\uB418\uC5B4 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, \uAC00\uB3C5\uC131\
  \uACFC \uC131\uB2A5\uC744 \uC704\uD574 \uCF54\uB4DC\uB97C \uC815\uB9AC\uD560 \uC218\
  \uB294 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
PowerShell에는 전용 리팩토링 도구가 내장되어 있지 않지만, 가독성과 성능을 위해 코드를 정리할 수는 있습니다. 너무 많은 일을 하고 있는 함수를 고려하고, 이를 명확성을 위해 어떻게 리팩토링할 수 있는지 살펴봅시다:

```PowerShell
function Get-InventoryData {
    # 데이터 검색과 포맷팅을 결합한 원래 함수
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# 별도의 함수로 리팩토링됨
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# 사용법
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

샘플 출력:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## 심층 분석
프로그래밍에서의 리팩토링은 소프트웨어 개발 초기부터 그 뿌리를 두고 있지만, 1990년대에 공식적인 실천으로 정립되었습니다. 마틴 파울러의 책 "Refactoring: Improving the Design of Existing Code"는 해당 주제에 대한 기념비적인 저작 중 하나로, 깨끗한 코드를 달성하기 위한 리팩토링의 중요성을 강조합니다.

PowerShell은 Eclipse나 Visual Studio와 같은 다른 언어를 위한 통합 개발 환경(IDE)에서와 같은 특정 리팩토링 도구를 제공하지 않지만, 여전히 수동으로 좋은 리팩토링 원칙을 실천할 수 있습니다. 리팩토링은 그저 코드를 변경하기 위해 변경하는 것이 아니라, 코드의 구조와 설계를 향상시키는 의도적이고, 동작을 보존하는 수정 작업임을 기억하는 것이 중요합니다.

PowerShell에서의 수동 리팩토링 대안으로는 해당 언어를 지원하는 IDE를 사용하는 것이 포함됩니다. 예를 들어 PowerShell 확장 기능을 갖춘 Visual Studio Code는 코드 포맷팅과 기본 리팩토링 기능과 같은 기능을 제공합니다. 보다 중대한 리팩토링을 위해서는 변경 사항이 기능성을 변경하지 않도록 보장하는 Pester 테스트를 활용하는 것을 고려할 수 있습니다.

또한, 리팩토링의 구현은 코드를 재사용 가능한 모듈이나 함수로 분리하는 것과 같은 보다 체계적인 변경을 포함할 수 있으며, 이는 DRY(Do Not Repeat Yourself) 원칙의 준수를 개선합니다. 기타 흔한 리팩토링 기법으로는 명확성을 위한 이름 변경, 중복 코드 제거, 조건 로직의 복잡성 감소 등이 있습니다.

## 참고
더 깊이 탐구하려면 다음 자료들을 참조하세요:

- 마틴 파울러의 리팩토링 책: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- 리팩토링된 코드와 함께 Pester 테스트하기: [Pester Testing Framework](https://pester.dev/)
- PowerShell 최고의 실천법: [The PowerShell Best Practices and Style Guide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
