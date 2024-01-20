---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

## 무엇이며 왜?

패턴에 일치하는 문자 삭제는 특정 문자나 문자열을 제거하는 프로그래밍 작업입니다. 비효율적인 공간 사용을 줄이고, 데이터를 정리하거나 표준화하기 위해 프로그래머들이 이를 사용합니다.

## 실시하기

아래의 코드는 PowerShell에서 'abc' 문자열에서 'b'를 찾아 제거합니다.

```PowerShell
$string = 'abc'
$pattern = 'b'
$string -replace $pattern, ''
```

위의 코드를 실행하면 'b'가 제거된 'ac'라는 결과를 얻을 수 있습니다.

다음 예제는 'banana' 문자열에서 모든 'a' 문자를 찾아 제거합니다.

```PowerShell
$string = 'banana'
$pattern = 'a'
$string -replace $pattern, ''
```

위의 코드를 실행하면 'a'가 모두 제거된 'bnn'라는 결과를 얻을 수 있습니다.

## 디테일한 내용

제거할 문자 또는 문자열을 지정하기 위해 정규 표현식을 사용하는 `-replace` 연산자의 기능은 PowerShell 3.0부터 시작되었습니다.

이 작업을 수행하는 대안적인 방법은 .NET의 `String.Replace` 메서드를 사용하는 것입니다.

```PowerShell
$string = 'abc'
$pattern = 'b'
$string.Replace($pattern, '')
```

이 방법은 `-replace` 연산자보다 덜 유연하지만 비슷한 결과를 얻을 수 있습니다.

제거할 문자 또는 패턴의 개수나 위치에 따라 `-replace` 연산자 또는 `String.Replace` 메서드 중 적절한 방법을 선택할 수 있습니다.

## 참고 자료

- 정규 표현식에 대한 더 많은 정보를 얻으려면 [여기](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)를 참조하십시오.
- `-replace` 연산자에 대한 더 많은 정보를 얻으려면 [여기](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)를 참조하십시오.
- `String.Replace` 메서드에 대한 더 많은 정보를 얻으려면 [여기](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)를 참조하십시오.