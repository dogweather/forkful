---
title:                "정규 표현식 사용하기"
aliases:
- ko/powershell/using-regular-expressions.md
date:                  2024-02-03T19:17:47.514377-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식(regex)은 주로 문자열 검색과 조작을 위해 사용되는 검색 패턴을 형성하는 문자의 시퀀스입니다. 프로그래머들은 데이터 검증, 구문 분석 및 변환과 같은 작업에 PowerShell에서 정규 표현식을 활용합니다. 이는 복잡한 패턴을 처리하는 데 있어 그 효율성과 유연성 때문입니다.

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
