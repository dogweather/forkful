---
title:                "문자열에서 따옴표 제거하기"
date:                  2024-01-26T03:41:47.290549-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 따옴표 제거하기"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
PowerShell에서 문자열에서 따옴표를 제거하면 텍스트 주위에 있는 단일(`'`) 또는 이중(`"`) 따옴표를 제거합니다. 프로그래머들은 특히 사용자 입력이나 파일 파싱을 다룰 때 처리, 비교 또는 출력 목적으로 문자열을 정리할 필요가 자주 있습니다.

## 방법:
`-replace` 연산자를 사용하여 문자열에서 따옴표를 제거할 수 있습니다. 방법은 다음과 같습니다:

```PowerShell
# 단일 따옴표 대체
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # 출력: Hello, World!

# 이중 따옴표 대체
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # 출력: Hello, World!
```

두 타입 모두에 대해:

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # regex 문자 클래스 사용에 주목
Write-Output $cleanString  # 출력: Hi there, she said.
```

콘솔에서의 샘플 출력은 다음과 같이 보일 것입니다:

```
Hello, World!
Hello, World!
Hi there, she said.
```

## 심층 분석
예전에, Microsoft의 눈동자가 되기 전 PowerShell이 있기 전에, Windows에서 텍스트 처리는 종종 제한된 기능을 가진 배치 스크립트의 영역이었습니다. PowerShell의 도입은 스크립팅을 훨씬 더 강력하게 만드는 강력한 문자열 조작 기능을 가져왔습니다.

`-replace` 외에도 `.Trim()` 메소드를 사용하여 문자열의 시작과 끝에서만 따옴표를 제거하는 것과 같은 대안이 존재하지만, 동일한 제어 또는 regex 지원을 제공하지 않습니다.

```PowerShell
# 시작과 끝에서 따옴표 제거를 위해 .Trim() 사용
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # 출력: Hello, World!
```

참고로, `-replace`는 내부적으로 regex를 사용하기 때문에, 이것을 사용할 때는 특수 문자가 대상이라면 이스케이프해야 한다는 점을 기억하세요. 따옴표 제거에 더 정밀한 제어가 필요하다면, `-replace`와 함께 regex로 깊이 파고드는 것이 방법이며, 이를 통해 엄청난 유연성을 얻을 수 있습니다.

## 참고
- PowerShell에서 regex에 대한 자세한 정보는 공식 문서를 확인하세요: [about_Regular_Expressions](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- 다른 문자열 메소드를 알아보세요: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.trim?view=net-6.0)
