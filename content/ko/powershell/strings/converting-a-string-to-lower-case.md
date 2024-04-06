---
date: 2024-01-20 17:39:11.080344-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PowerShell\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uBC29\uBC95\
  \uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4. `.ToLower()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\
  \uC6A9\uD558\uBA74 \uB3FC\uC694. \uC544\uB798 \uC608\uC81C \uCF54\uB4DC\uB97C \uC2E4\
  \uD589\uD574\uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.186520-06:00'
model: gpt-4-1106-preview
summary: "`.ToLower()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uBA74 \uB3FC\uC694\
  ."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to (어떻게 하나요?)
PowerShell에서 문자열을 소문자로 바꾸는 방법은 간단합니다. `.ToLower()` 메서드를 사용하면 돼요. 아래 예제 코드를 실행해보세요.

```PowerShell
# 변수에 문자열 할당
$str = "HELLO, WORLD!"

# 소문자로 변환
$lowerStr = $str.ToLower()

# 결과 출력
$lowerStr
```

실행 결과는 다음과 같이 나와야 합니다:

```
hello, world!
```

가끔은 전체 파이프라인을 통해 여러 항목에 적용할 수 있습니다:

```PowerShell
# 배열의 모든 문자열을 소문자로 변환
$strings = "FIRST", "SECOND", "THIRD"
$strings | ForEach-Object { $_.ToLower() }
```

결과:

```
first
second
third
```

## Deep Dive (심층 분석)
과거에는 대소문자 변환 작업이 컴퓨터에 많은 자원을 소모했어요. 하지만 현대의 PowerShell은 이 작업을 빠르고 효율적으로 합니다. 대안으로는 `$str.toLowerInvariant()`가 있습니다. 이것은 특정 문화권에 관계없이 변환을 실행합니다. 이해하려면 유니코드와 문화권이 어떻게 문자 변환에 영향을 미치는지 알아야 하죠. 소문자 변환은 유니코드 표준에 기반하여 작동하며, 각 문자는 고유 코드 포인트를 가지고 있어요.

## See Also (관련 링크)
- [String Methods - Microsoft .NET Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
