---
title:                "문자열의 길이 찾기"
html_title:           "PowerShell: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜? 
문자열의 길이를 파악하는 것은 프로그래머에게 중요한 작업입니다. 문자열의 길이를 알면 자료를 보다 효율적으로 다룰 수 있고, 오류를 방지할 수 있기 때문입니다.

## 하는 법:
```PowerShell
$str = "안녕하세요!"
Write-Host "문자열의 길이는" $str.Length "입니다."
```

```PowerShell
$str = "Hello World!"
Write-Host "The length of the string is" $str.Length "."
```

```PowerShell
$str = "Hola Mundo!"
Write-Host "La longitud de la cadena es" $str.Length "."
```

## 더 깊게 들어가기:
문자열의 길이를 알아내는 것은 매우 기본적이고 기본적인 프로그래밍 작업입니다. 모든 프로그래밍 언어에서 사용할 수 있으며, 자료의 길이를 파악하는 다양한 방법이 있습니다. 예를 들어, 문자열의 길이를 파악하는 다른 방법으로는 ‘substring’ 함수를 사용하는 방법이 있습니다. 이 함수는 원하는 위치에서 소스 문자열로부터 지정된 길이만큼의 문자열을 추출하는 역할을 합니다. 

## 관련 정보:
- [Microsoft PowerShell 문서](https://docs.microsoft.com/ko-kr/powershell/scripting/learn/ps101/05-text?view=powershell-7.1)
- [DevDojo: String Length in PowerShell](https://devdojo.com/mattspaulding/string-length-in-powershell)