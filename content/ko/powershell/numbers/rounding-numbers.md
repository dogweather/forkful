---
date: 2024-01-26 03:46:12.938099-07:00
description: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uAC12\uC744 \uAC00\uC7A5 \uAC00\
  \uAE4C\uC6B4 \uC815\uC218\uB098 \uC9C0\uC815\uB41C \uC18C\uC218\uC810 \uC790\uB9AC\
  \uB85C \uC870\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uB2E8\uC21C\
  \uD654\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uAC70\uB098, \uACC4\uC0B0\
  \ \uB3C4\uC911 \uD2B9\uC815 \uC218\uD559\uC801 \uC694\uAD6C \uC0AC\uD56D\uC744 \uB9CC\
  \uC871\uC2DC\uD0A4\uAE30 \uC704\uD574 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD569\
  \uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.449539
model: gpt-4-0125-preview
summary: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uAC12\uC744 \uAC00\uC7A5 \uAC00\uAE4C\
  \uC6B4 \uC815\uC218\uB098 \uC9C0\uC815\uB41C \uC18C\uC218\uC810 \uC790\uB9AC\uB85C\
  \ \uC870\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uB2E8\uC21C\uD654\
  \uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uAC70\uB098, \uACC4\uC0B0 \uB3C4\
  \uC911 \uD2B9\uC815 \uC218\uD559\uC801 \uC694\uAD6C \uC0AC\uD56D\uC744 \uB9CC\uC871\
  \uC2DC\uD0A4\uAE30 \uC704\uD574 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD569\uB2C8\
  \uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
숫자 반올림은 값을 가장 가까운 정수나 지정된 소수점 자리로 조정하는 것에 관한 것입니다. 프로그래머들은 데이터를 단순화하고, 가독성을 높이거나, 계산 도중 특정 수학적 요구 사항을 만족시키기 위해 숫자를 반올림합니다.

## 방법:
PowerShell에서는 반올림을 위한 몇 가지 편리한 cmdlet과 메소드가 있습니다:

- Math 클래스의 `Round()` 메소드
```PowerShell
[Math]::Round(15.68) # 16으로 반올림
```
- 소수점 지정:
```PowerShell
[Math]::Round(15.684, 2) # 15.68로 반올림
```
- `Ceiling()`과 `Floor()`, 항상 올리거나 내림:
```PowerShell
[Math]::Ceiling(15.2) # 16으로 올림
[Math]::Floor(15.9) # 15로 내림
```

## 심도 있는 탐구
숫자 반올림은 신입이 아니며, 고대 시대부터 교역, 과학, 시간 측정에 유용하게 사용되었습니다. PowerShell에 대해 이야기하자면, `[Math]::Round()`는 기본적으로 "은행가의 반올림"을 따릅니다. 여기서 0.5는 가장 가까운 짝수로 가며, 통계 작업에서의 편향을 줄입니다.

하지만 `[Math]` 메소드에만 국한되지 않습니다. 더 많은 제어를 원하십니까? 중간점이 어떻게 처리되는지 설정할 수 있는 `[System.Math]::Round(숫자, 자릿수, MidpointRounding)`을 확인해 보세요: 0에서 멀어지거나 짝수로 (즉, 은행가의 반올림) 처리됩니다.

또 다른 관점: `System.Globalization.CultureInfo` 객체입니다. 국제 숫자를 다룰 때 지역별 서식 및 반올림 선호도와 관련하여 도움을 줍니다.

## 참고 자료
- Math 메소드에 대한 Microsoft의 공식 문서: [링크](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- .NET에서의 소수점 반올림 특성: [링크](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- StackOverflow에서의 반올림에 대한 토론: [링크](https://stackoverflow.com/questions/tagged/rounding+powershell)
