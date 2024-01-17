---
title:                "텍스트 파일 읽기"
html_title:           "PowerShell: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?: 
텍스트 파일을 읽는 것은 파일 안에 저장된 데이터를 읽고 처리하는 것을 의미합니다. 프로그래머들은 이를 자주 사용하는데 이는 파일로 정보를 저장하는 것이 더 쉽고 효과적이기 때문입니다.

## 하는 법: 
```PowerShell
# 파일 읽기
Get-Content example.txt

# 파일을 변수에 저장하기
$contents = Get-Content example.txt
Write-Host $contents
```

파일을 읽는 것은 매우 간단합니다. `Get-Content` 명령을 사용하면 파일 내용을 출력할 수 있습니다. 이를 변수에 저장하면 해당 변수를 사용하여 파일 내용을 조작하거나 출력할 수 있습니다.

### 출력 예시:
```
Hello, world! This is an example text file.
```

## 깊이 파헤치기: 
파일을 읽는 방식은 이전부터 사용되어온 전통적인 방법입니다. 하지만 오늘날에는 다양한 파일 형식이 등장하면서 다양한 데이터 처리 방법이 나타나기도 합니다. 파일을 읽는 대안으로는 JavaScript와 같은 웹 프로그래밍 언어에서 사용되는 FileReader API 등이 있습니다.

파일을 읽는 방식은 기본적으로 파일의 인코딩 방식에 따라 다릅니다. PowerShell의 `Get-Content` 명령에서는 기본적으로 "UTF-8" 인코딩을 사용하기 때문에 다른 인코딩 방식을 사용하는 파일은 인코딩을 직접 명시해야 합니다.

## 관련 문서: 
- [PowerShell Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [FileReader API documentation](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- [파일 인코딩과 관련한 이슈](https://ss64.com/ps/add-content.html)