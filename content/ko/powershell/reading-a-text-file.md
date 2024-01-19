---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜그럴까?
텍스트 파일 읽기는 파일의 내용을 코드로 해석하는 프로그래밍 작업입니다. 이를 통해 프로그래머들은 데이터를 처리하고 정보를 추출할 수 있습니다.

## 어떻게:
다음은 PowerShell로 텍스트 파일을 읽는 간단한 코드 예입니다:

```PowerShell
$file = Get-Content -Path C:\Example\myfile.txt
```
이 스크립트는 "myfile.txt"라는 텍스트 파일의 내용을 "$file" 변수에 저장합니다. 저장된 내용은 이후 사용을 위해 참조할 수 있습니다.

```PowerShell
$file | Foreach-Object { $_ }
```
이 코드는 각 라인을 순회하며 출력합니다.

## 깊이 들어가기:
파워셸에서 텍스트 파일 읽기는 오래된 기능 중 하나입니다. 이는 윈도우에서 작업을 자동화하고 관리하는데 꽤 유용합니다. Get-Content 명령어는 파일의 내용을 대화식으로 읽을 수 있게 합니다.

하지만, 다른 대안들을 살펴볼 만하요. 예를 들어, [StreamReader object](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)를 사용해 큰 텍스트 파일을 점진적으로 읽는 것입니다. 이 방법은 메모리 사용량을 줄이는데 도움이 됩니다.

텍스트 파일을 읽는 방법에 대한 세부 사항 중 하나는 파일 인코딩이다. 인코딩이 일치하지 않으면 읽기 시도가 실패하거나 파일의 내용이 올바르게 보여지지 않을 수 있습니다.

## 참고 링크:
- [PowerShell Documentation: Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [StreamReader Object in .NET API](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [File Encoding in PowerShell](https://devblogs.microsoft.com/powershell/powershell-default-encoding-update-utf-8)