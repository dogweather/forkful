---
date: 2024-01-20 17:41:02.444360-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.574854-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

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
