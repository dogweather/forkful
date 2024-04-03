---
date: 2024-01-26 04:44:06.667947-07:00
description: "\uBC29\uBC95: PowerShell\uC740 \uBCF5\uC18C\uC218\uB97C \uB0B4\uC7A5\
  \ \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC790\uCCB4 \uD574\uACB0\uCC45\
  \uC744 \uB9CC\uB4E4\uAC70\uB098 .NET\uC758 `System.Numerics.Complex`\uB97C \uC0AC\
  \uC6A9\uD574\uC57C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.534780-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uBCF5\uC18C\uC218\uB97C \uB0B4\uC7A5 \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC790\uCCB4 \uD574\uACB0\uCC45\uC744 \uB9CC\uB4E4\
  \uAC70\uB098 .NET\uC758 `System.Numerics.Complex`\uB97C \uC0AC\uC6A9\uD574\uC57C\
  \ \uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
PowerShell은 복소수를 내장 지원하지 않으므로, 자체 해결책을 만들거나 .NET의 `System.Numerics.Complex`를 사용해야 합니다.

```PowerShell
# .NET을 사용해 복소수 만들기
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# 복소수 생성
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# 두 복소수 더하기
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# 두 복소수 곱하기
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# 결과 표시
"합: $sum"
"곱: $product"
```
출력:
```
합: (4, 6)
곱: (-5, 10)
```

## 심화 학습
복소수는 16세기에 실수의 영역에서 해답이 없는 방정식을 풀기 위해 개발되었습니다. 이제 현대 수학의 기초가 되었습니다.

PowerShell이 복소수 지원을 위해 .NET에 의존한다는 것은 성능이 확실함을 의미합니다. 대안으로는 타사 라이브러리나 복소수가 기본 데이터 유형인 파이썬과 같은 다른 프로그래밍 언어가 있습니다.

## 참고
- [System.Numerics.Complex 구조체](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [파이썬에서의 복소수 산술](https://docs.python.org/3/library/cmath.html)
