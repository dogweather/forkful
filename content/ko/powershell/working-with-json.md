---
title:                "JSON과 함께 작업하기"
html_title:           "PowerShell: JSON과 함께 작업하기"
simple_title:         "JSON과 함께 작업하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
JSON 작업이란 무엇인가요? 프로그래머들이 이를 왜 하는 걸까요? 

JSON이란 "JavaScript Object Notation"의 약자로, 데이터를 저장하고 교환하는 데 사용되는 텍스트 형식입니다. JSON은 간결하고 가독성이 좋으며 다양한 프로그래밍 언어에서 쉽게 사용할 수 있어서 많은 프로그래머들이 이를 선호합니다.

## 어떻게: 
```PowerShell
# JSON 파일 읽어오기
Get-Content example.json

# JSON 형식의 데이터 만들기
$person = [pscustomobject] @{
    name = "Jane Doe"
    age = 30
    address = @{
        city = "Seoul"
        country = "South Korea"
    }
}

# 데이터를 JSON 형식의 파일로 저장하기
$person | ConvertTo-Json | Out-File example.json

# 저장된 파일 읽어오기
Get-Content example.json | ConvertFrom-Json
```

## 깊게 파헤치기:
JSON은 1990년대 중반에 개발된 방식으로, 웹 어플리케이션에서 사용하기 위해 제안되었습니다. 이전에는 XML이 주로 사용되었지만, JSON 형식이 더 간결하고 가볍기 때문에 더 많이 사용되고 있습니다. 또한 다양한 프로그래밍 언어에서 지원되므로 유연하게 사용하기 쉽습니다. 대안으로는 YAML이 있지만, JSON이 더 쉽고 직관적이어서 더 인기 있습니다.  

JSON 파일을 만들려면 데이터의 구조를 먼저 이해해야 합니다. 즉, 객체, 배열, 문자열, 숫자 등의 데이터 유형을 알고 있어야 합니다. 또한 PowerShell에서 지원하는 ```[pscustomobject]```를 사용하여 쉽게 JSON 형식의 데이터를 만들 수 있습니다. 마지막으로, ConvertTo-Json 명령어를 사용하여 데이터를 JSON 형식으로 변환하고 Out-File 명령어를 사용하여 파일로 저장할 수 있습니다.

## 관련 자료 보기:
- PowerShell에서 JSON 다루기 - https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json?view=powershell-7.1
- 개발자를 위한 JSON 소개 - https://www.json.org/json-ko.html
- JSON vs XML - https://www.baeldung.com/json-vs-xml