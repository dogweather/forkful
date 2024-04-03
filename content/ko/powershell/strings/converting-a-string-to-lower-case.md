---
date: 2024-01-20 17:39:11.080344-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD55C\
  \uB2E4\uB294 \uAC74, \uBB38\uC790\uC5F4 \uB0B4\uC758 \uBAA8\uB4E0 \uB300\uBB38\uC790\
  \uB97C \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uC8FC\
  \uB85C \uB370\uC774\uD130\uB97C \uC77C\uAD00\uB418\uAC8C \uCC98\uB9AC\uD558\uAC70\
  \uB098, \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBCC4\uD558\uC9C0 \uC54A\uB294 \uAC80\
  \uC0C9\uC744 \uC218\uD589\uD560 \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.524417-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC74, \uBB38\uC790\uC5F4 \uB0B4\uC758 \uBAA8\uB4E0 \uB300\uBB38\uC790\uB97C\
  \ \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
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
