---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:47.514377-07:00
description: "\uC5B4\uB5BB\uAC8C: PowerShell\uC5D0\uC11C\uB294 `-match`, `-replace`,\
  \ `-split` \uC5F0\uC0B0\uC790 \uB4F1\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC73C\uB85C \uC791\uC5C5\uC744 \uC218\uD589\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uBA87 \uAC00\uC9C0 \uC608\uB97C \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:57.189499-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C\uB294 `-match`, `-replace`, `-split` \uC5F0\uC0B0\
  \uC790 \uB4F1\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC815\uADDC \uD45C\uD604\uC2DD\uC73C\
  \uB85C \uC791\uC5C5\uC744 \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 어떻게:
PowerShell에서는 `-match`, `-replace`, `-split` 연산자 등을 사용하여 정규 표현식으로 작업을 수행할 수 있습니다. 몇 가지 예를 살펴보겠습니다:

### 문자열이 패턴과 일치하는지 확인하기 위해 `-match` 사용
이 연산자는 패턴이 문자열 내에서 발견되면 `$true`를, 그렇지 않으면 `$false`를 반환합니다.

```powershell
"hello world" -match "\w+orld"
# 출력: True
```

### 일치하는 값 추출하기
자동 변수 `$matches`에 접근하여 일치하는 값을 추출할 수 있습니다.

```powershell
if ("I have 100 apples" -match "\d+") {
    "발견된 숫자: " + $matches[0]
}
# 출력: 발견된 숫자: 100
```

### 치환을 위해 `-replace` 사용하기
`-replace` 연산자는 특정 치환 문자열로 패턴의 모든 발생을 대체합니다.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# 출력: foo qux qux
```

### `-split`로 문자열 나누기
정규 표현식 패턴을 기반으로 문자열을 여러 부분 문자열의 배열로 분할합니다.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# 출력: The quick brown fox jumps
```

### 고급 패턴 매칭
PowerShell은 `Matches()`, `Replace()`, `Split()`과 같은 메소드에 접근할 수 있는 `[regex]` 클래스를 통해 더 복잡한 정규 표현식 작업도 지원합니다.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# 출력: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# 출력: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# 출력: one two three four
```

이 예제들은 데이터 조작과 패턴 매칭에 있어 PowerShell에서 정규 표현식의 강력함과 다양성을 보여줍니다. 정규 표현식을 활용함으로써 프로그래머들은 복잡한 텍스트 처리를 효율적으로 수행할 수 있습니다.
