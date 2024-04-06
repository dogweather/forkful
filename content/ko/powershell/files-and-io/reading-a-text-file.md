---
date: 2024-01-20 17:55:08.945475-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uD30C\uC6CC\uC178\uC5D0\uC11C \uD14D\uC2A4\
  \uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uC2A4\uD06C\uB9BD\uD2B8 \uC5B8\
  \uC5B4\uAC00 \uB4F1\uC7A5\uD55C \uCD08\uAE30\uBD80\uD130 \uC788\uC5C8\uB358 \uAE30\
  \uB2A5\uC785\uB2C8\uB2E4. `Get-Content`\uB294 \uAC00\uC7A5 \uC77C\uBC18\uC801\uC778\
  \ \uBC29\uBC95\uC778\uB370, \uC774 \uBA85\uB839\uC5B4\uB294 \uD30C\uC77C\uC744 \uD55C\
  \ \uBC88\uC5D0 \uBA54\uBAA8\uB9AC\uB85C \uC77D\uC5B4\uB4E4\uC774\uAE30 \uB54C\uBB38\
  \uC5D0 \uD070 \uD30C\uC77C\uC744 \uB2E4\uB8F0 \uB54C\uB294 \uC131\uB2A5 \uBB38\uC81C\
  \uAC00 \uBC1C\uC0DD\uD560 \uC218\uB3C4 \uC788\uC5B4\uC694.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.223187-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uD30C\uC6CC\uC178\uC5D0\uC11C \uD14D\uC2A4\uD2B8\
  \ \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uC2A4\uD06C\uB9BD\uD2B8 \uC5B8\uC5B4\
  \uAC00 \uB4F1\uC7A5\uD55C \uCD08\uAE30\uBD80\uD130 \uC788\uC5C8\uB358 \uAE30\uB2A5\
  \uC785\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (어떻게:)
```PowerShell
# 파일 전체를 한번에 읽기
Get-Content -Path "example.txt"

# 각 줄을 개별적으로 처리
Get-Content -Path "example.txt" | ForEach-Object {
  # 여기서 $_ 변수는 현재 줄을 나타냅니다.
  Write-Output "Reading line: $_"
}

# 출력 예시
Reading line: 첫 번째 줄입니다.
Reading line: 두 번째 줄입니다.
...
```

## Deep Dive (심화 학습)
파워셸에서 텍스트 파일을 읽는 것은 스크립트 언어가 등장한 초기부터 있었던 기능입니다. `Get-Content`는 가장 일반적인 방법인데, 이 명령어는 파일을 한 번에 메모리로 읽어들이기 때문에 큰 파일을 다룰 때는 성능 문제가 발생할 수도 있어요. 대안으로 `[System.IO.File]::ReadLines()` 같은 .NET 메서드를 사용할 수도 있고, `StreamReader` 객체를 통해 더 세밀한 제어가 가능합니다.

```PowerShell
# .NET의 ReadLines 활용
[System.IO.File]::ReadLines("example.txt")

# StreamReader 사용 예
$reader = [System.IO.StreamReader]::new("example.txt")
while($line = $reader.ReadLine()){
  Write-Output "Reading line: $line"
}
$reader.Close()
```

이런 방식들은 파일 크기가 큰 경우나 메모리 사용을 제한해야 할 때 유리합니다. 스트림을 통해 한 줄씩 읽기 때문에 전체 파일을 메모리에 로드할 필요가 없죠.

## See Also (관련 링크)
- 공식 PowerShell 문서에서 Get-Content에 대해 자세히 알아보기: [Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.2)
- .NET의 파일 시스템 메서드에 대한 설명: [System.IO Namespace](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=net-6.0)
- StackOverflow의 PowerShell 관련 질문들: [PowerShell Questions](https://stackoverflow.com/questions/tagged/powershell)
