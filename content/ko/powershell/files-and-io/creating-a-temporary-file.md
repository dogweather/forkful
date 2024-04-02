---
date: 2024-01-20 17:41:02.444360-07:00
description: "\uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC784\uC2DC \uD30C\uC77C\uC744\
  \ \uB9CC\uB4DC\uB294 \uAC74 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C\
  \ \uC800\uC7A5\uD558\uAE30 \uC704\uD574\uC11C\uC608\uC694. \uC791\uC5C5 \uC911\uAC04\
  \uC5D0 \uD544\uC694\uD55C \uB370\uC774\uD130\uB97C \uBCF4\uAD00\uD558\uAC70\uB098\
  , \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8 \uAC04 \uB370\uC774\uD130\uB97C \uC804\uC1A1\
  \uD558\uB294 \uB370 \uC4F0\uC9C0\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.574854-06:00'
model: gpt-4-1106-preview
summary: "\uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\
  \uB4DC\uB294 \uAC74 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C \uC800\
  \uC7A5\uD558\uAE30 \uC704\uD574\uC11C\uC608\uC694. \uC791\uC5C5 \uC911\uAC04\uC5D0\
  \ \uD544\uC694\uD55C \uB370\uC774\uD130\uB97C \uBCF4\uAD00\uD558\uAC70\uB098, \uC751\
  \uC6A9 \uD504\uB85C\uADF8\uB7A8 \uAC04 \uB370\uC774\uD130\uB97C \uC804\uC1A1\uD558\
  \uB294 \uB370 \uC4F0\uC9C0\uC694."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## What & Why? (무엇과 왜?)
프로그래머가 임시 파일을 만드는 건 데이터를 일시적으로 저장하기 위해서예요. 작업 중간에 필요한 데이터를 보관하거나, 응용 프로그램 간 데이터를 전송하는 데 쓰지요.

## How to: (어떻게:)
```PowerShell
# 임시 파일 생성
$tempFile = [System.IO.Path]::GetTempFileName()

# 임시 파일에 데이터 쓰기
"임시 데이터" | Out-File -FilePath $tempFile

# 임시 파일 읽기
Get-Content -Path $tempFile

# 임시 파일 삭제
Remove-Item -Path $tempFile
```

샘플 출력:
```
임시 데이터
```

## Deep Dive (깊이 들여다보기)
임시 파일은 윈도에서 'Temp' 폴더를 사용해 만들어져요. 일반적으로 'C:\Users\<UserName>\AppData\Local\Temp' 경로에 있죠. 이것들은 시스템 재부팅이나 프로그램 종료 시 자동으로 삭제될 수 있어요.

역사적으로, 임시 파일은 디스크 기반 저장 공간이 부족할 때 많이 쓰였어요. 메모리에 저장할 수 없는 대용량 데이터를 처리할 때 임시 파일이 유용했죠.

대체 방법으로, 메모리 내 데이터 스트림(예: PowerShell의 `[System.IO.MemoryStream]` 객체)을 사용할 수도 있어요. 하지만, 파일이 필요한 경우에는 임시 파일 생성이 최선의 선택일 수 있어요.

## See Also (더 보기)
- [System.IO.Path 클래스](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netframework-4.8)
- [PowerShell Out-File 커맨드](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file?view=powershell-7.1)
- [PowerShell Get-Content 커맨드](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
