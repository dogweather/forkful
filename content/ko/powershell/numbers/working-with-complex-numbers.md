---
date: 2024-01-26 04:44:06.667947-07:00
description: "\uBCF5\uC18C\uC218(complex numbers), \uC989 \uC2E4\uC218\uBD80\uC640\
  \ \uD5C8\uC218\uBD80(\uC608: 3 + 4i)\uB97C \uAC00\uC9C4 \uC218\uB294 \uACF5\uD559\
  , \uBB3C\uB9AC\uD559, \uB370\uC774\uD130 \uACFC\uD559\uACFC \uAC19\uC740 \uBD84\uC57C\
  \uC5D0\uC11C \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB97C \uC2DC\uBBAC\uB808\uC774\uC158, \uC2E0\uD638 \uCC98\uB9AC\
  , \uD2B9\uC815 \uC720\uD615\uC758 \uC218\uD559 \uBB38\uC81C \uD574\uACB0\uC5D0 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.534780-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218(complex numbers), \uC989 \uC2E4\uC218\uBD80\uC640 \uD5C8\
  \uC218\uBD80(\uC608: 3 + 4i)\uB97C \uAC00\uC9C4 \uC218\uB294 \uACF5\uD559, \uBB3C\
  \uB9AC\uD559, \uB370\uC774\uD130 \uACFC\uD559\uACFC \uAC19\uC740 \uBD84\uC57C\uC5D0\
  \uC11C \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774\uB97C \uC2DC\uBBAC\uB808\uC774\uC158, \uC2E0\uD638 \uCC98\uB9AC, \uD2B9\
  \uC815 \uC720\uD615\uC758 \uC218\uD559 \uBB38\uC81C \uD574\uACB0\uC5D0 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
복소수(complex numbers), 즉 실수부와 허수부(예: 3 + 4i)를 가진 수는 공학, 물리학, 데이터 과학과 같은 분야에서 필수적입니다. 프로그래머들은 이를 시뮬레이션, 신호 처리, 특정 유형의 수학 문제 해결에 사용합니다.

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
