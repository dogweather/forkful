---
title:                "임시 파일 만들기"
html_title:           "PowerShell: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
임시 파일을 생성하는 것은 시스템의 일시적인 저장 공간에 파일을 만드는 것을 뜻합니다. 프로그래머들은 데이터를 임시로 저장하거나 다른 프로그램과의 상호 작용을 위해 임시 파일을 생성합니다.

## 어떻게:
```PowerShell
# 새로운 임시 파일 생성하기
New-TemporaryFile

# 임시 파일의 경로 출력하기
$tempFile = New-TemporaryFile
$tempFile.FullName
```

```
C:\Users\Username\AppData\Local\Temp\tmp1F4B.tmp
```

## 깊이 파고들기:
본래 임시 파일은 오래된 운영 체제의 시스템 리소스를 관리하기 위한 방법으로 만들어졌습니다. 현재는 많은 프로그래밍 언어에서 임시 파일을 생성할 수 있지만, PowerShell의 New-TemporaryFile 명령어는 간편하고 효율적인 방법을 제공합니다. 임시 파일 생성을 위해 .NET Framework의 System.IO.Path 클래스를 활용하여 작동합니다.

## 참고 자료:
- [PowerShell New-TemporaryFile 명령어 문서](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-temporaryfile?view=powershell-7.1)
- [.NET Framework의 System.IO.Path 클래스 문서](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)