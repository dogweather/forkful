---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:47.255976-07:00
description: "\uBC29\uBC95: \uD30C\uC6CC\uC258\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\
  \uC744 \uC0DD\uC131\uD558\uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uC544\uC8FC\
  \ \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uB9C8\uBC95\uC744 \uBD80\uB9AC\uB294 \uBC29\uBC95\
  \uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4: **\uC5F0\uAD00 \uBC30\uC5F4\
  \ \uC0DD\uC131:**."
lastmod: '2024-03-13T22:44:55.533313-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC6CC\uC258\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\uC744 \uC0DD\uC131\
  \uD558\uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uC544\uC8FC \uAC04\uB2E8\uD569\
  \uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
파워쉘에서 연관 배열을 생성하고 사용하는 것은 아주 간단합니다. 마법을 부리는 방법은 다음과 같습니다:

**연관 배열 생성:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Engineer"
```

이 코드 스니펫은 세 개의 키-값 쌍을 가진 연관 배열을 생성합니다.

**값 접근하기:**

값을 얻기 위해 키를 참조하세요:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**샘플 출력:**

```
Alex
```

**데이터 추가 또는 수정하기:**

새로운 쌍을 추가하거나 기존의 것을 수정하기 위해서는 키를 사용하세요:

```PowerShell
$myAssociativeArray["location"] = "New York" # 새로운 키-값 쌍 추가
$myAssociativeArray["job"] = "Senior Engineer" # 기존 쌍 수정
```

**연관 배열 반복하기:**

키와 값에 대해 이렇게 반복하세요:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**샘플 출력:**

```
name : Alex
age : 25
job : Senior Engineer
location : New York
```

## 심층 탐구
연관 배열의 개념은 많은 프로그래밍 언어에서 공통적으로 나타나며, 언어에 따라 딕셔너리, 맵, 또는 해시 테이블 등으로 불립니다. 파워쉘에서는 해시 테이블로 구현되어 키를 검색하고 데이터를 저장하며, 고유 키의 컬렉션을 유지하는 데 아주 효율적입니다.

역사적으로, 연관 배열은 각 항목을 전체 컬렉션을 순회하지 않고도 키를 사용하여 빠르게 검색할 수 있는 수단을 제공합니다. 데이터 검색과 수정의 효율성 때문에 다양한 작업에 선호되는 선택지가 되지만, 정렬 유지와 같은 제한 사항들이 있으며, 이 경우에는 정렬된 딕셔너리나 커스텀 객체가 더 나은 대안이 될 수 있습니다.

그럼에도 불구하고, 파워쉘의 연관 배열/해시 테이블은 놀라울 정도로 유연하며 스크립팅에 강력한 도구입니다. 동적 데이터 저장을 가능하게 하며, 구성, 데이터 조작, 구조화된 데이터 형식이 필요한 곳이라면 어디서든 유용하게 사용될 수 있습니다. 형식적인 클래스 정의의 부담 없이 말이죠. 단지 기억하세요, 연관 배열은 키 기반 검색에 완벽하지만, 복잡한 데이터 구조를 다루거나 특정 순서를 유지해야 하는 작업에는 파워쉘 내의 다른 데이터 타입이나 커스텀 객체를 탐색하는 것이 좋을 수 있습니다.
