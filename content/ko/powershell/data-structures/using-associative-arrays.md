---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:47.255976-07:00
description: "\uD30C\uC6CC\uC258\uC5D0\uC11C\uC758 \uC5F0\uAD00 \uBC30\uC5F4, \uB610\
  \uB294 \uD574\uC2DC \uD14C\uC774\uBE14\uC774\uB098 \uB515\uC154\uB108\uB9AC\uB77C\
  \uACE0\uB3C4 \uC54C\uB824\uC9C4 \uC774 \uAE30\uC220\uC740 \uB370\uC774\uD130\uB97C\
  \ \uD0A4-\uAC12 \uC30D\uC73C\uB85C \uC800\uC7A5\uD558\uAC8C \uD574\uC8FC\uC5B4,\
  \ \uB370\uC774\uD130 \uAC80\uC0C9\uC744 \uAC04\uACB0\uD558\uACE0 \uD6A8\uC728\uC801\
  \uC73C\uB85C \uB9CC\uB4ED\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC774\uB97C \uD1B5\uD574 \uAD00\uB828 \uB370\uC774\uD130\uB97C \uD0A4\uB97C \uD1B5\
  \uD574 \uC27D\uAC8C \uC811\uADFC\uD560 \uC218 \uC788\uB294 \uBC29\uC2DD\uC73C\uB85C\
  \ \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.533313-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC6CC\uC258\uC5D0\uC11C\uC758 \uC5F0\uAD00 \uBC30\uC5F4, \uB610\uB294\
  \ \uD574\uC2DC \uD14C\uC774\uBE14\uC774\uB098 \uB515\uC154\uB108\uB9AC\uB77C\uACE0\
  \uB3C4 \uC54C\uB824\uC9C4 \uC774 \uAE30\uC220\uC740 \uB370\uC774\uD130\uB97C \uD0A4\
  -\uAC12 \uC30D\uC73C\uB85C \uC800\uC7A5\uD558\uAC8C \uD574\uC8FC\uC5B4, \uB370\uC774\
  \uD130 \uAC80\uC0C9\uC744 \uAC04\uACB0\uD558\uACE0 \uD6A8\uC728\uC801\uC73C\uB85C\
  \ \uB9CC\uB4ED\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 무엇 & 왜?

파워쉘에서의 연관 배열, 또는 해시 테이블이나 딕셔너리라고도 알려진 이 기술은 데이터를 키-값 쌍으로 저장하게 해주어, 데이터 검색을 간결하고 효율적으로 만듭니다. 프로그래머들은 이를 통해 관련 데이터를 키를 통해 쉽게 접근할 수 있는 방식으로 저장하기 위해 사용합니다.

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
