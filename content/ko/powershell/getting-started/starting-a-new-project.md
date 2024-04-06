---
date: 2024-01-20 18:04:21.456880-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C\uD558\uAE30) PowerShell\uC740 \uC720\uB2C9\
  \uC2A4\uC758 \uC178 \uC2A4\uD06C\uB9BD\uD2B8\uC640 \uBE44\uC2B7\uD558\uC9C0\uB9CC\
  , .NET \uAE30\uBC18\uC73C\uB85C \uB9CC\uB4E4\uC5B4\uC838\uC11C \uB354 \uAC15\uB825\
  \uD55C \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB9C8\uC774\uD06C\uB85C\
  \uC18C\uD504\uD2B8\uB294 2006\uB144\uC5D0 \uCCAB \uC120\uC744 \uBCF4\uC778 \uC774\
  \uD6C4\uB85C \uAFB8\uC900\uD788 \uC5C5\uB370\uC774\uD2B8\uD558\uBA70 \uC544\uC9C1\
  \uAE4C\uC9C0 \uAC1C\uBC1C \uC911\uC785\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uCEE4\uB9E8\
  \uB4DC\uB77C\uC778 \uC778\uD130\uD398\uC774\uC2A4\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.820443-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C\uD558\uAE30) PowerShell\uC740 \uC720\uB2C9\uC2A4\uC758\
  \ \uC178 \uC2A4\uD06C\uB9BD\uD2B8\uC640 \uBE44\uC2B7\uD558\uC9C0\uB9CC, .NET \uAE30\
  \uBC18\uC73C\uB85C \uB9CC\uB4E4\uC5B4\uC838\uC11C \uB354 \uAC15\uB825\uD55C \uAE30\
  \uB2A5\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to: (어떻게하기)
```PowerShell
# 새 디렉터리 생성
New-Item -ItemType Directory -Path "C:\MyNewProject"

# 프로젝트 디렉터리로 이동
Set-Location -Path "C:\MyNewProject"

# 간단한 스크립트 파일 생성
New-Item -ItemType File -Path ".\MyScript.ps1"

# 스크립트에 'Hello, World!' 출력 코드 추가
Add-Content -Path ".\MyScript.ps1" -Value "Write-Host 'Hello, World!'"

# 스크립트 실행
.\MyScript.ps1
```
Sample Output:
```
Hello, World!
```

## Deep Dive (심층 분석)
PowerShell은 유닉스의 셸 스크립트와 비슷하지만, .NET 기반으로 만들어져서 더 강력한 기능을 제공합니다. 마이크로소프트는 2006년에 첫 선을 보인 이후로 꾸준히 업데이트하며 아직까지 개발 중입니다. 이러한 커맨드라인 인터페이스는 스크립팅뿐만 아니라 시스템 관리 작업에도 매우 유용합니다.

새 프로젝트를 시작할 때, PowerShell을 사용하면 단 몇 줄의 코드로 필요한 폴더와 파일을 생성할 수 있습니다. 그리고 `Write-Host`는 테스트나 디버깅 시 화면에 빠르게 결과를 보여줄 때 사용할 수 있습니다. 대안으로는 Python, Bash 등 다른 스크립트 언어가 있지만, PowerShell은 윈도우 환경과 더욱 밀접하게 연동되어 있습니다. 

예를 들어, PowerShell은 `.ps1` 파일 확장명을 사용하여 스크립트를 저장하고, 'ExecutionPolicy'의 적절한 설정을 통해 보안을 제어할 수 있습니다. 선택적으로 Visual Studio Code와 같은 IDE를 사용하여 보다 향상된 코드 작성 및 디버깅 환경을 경험할 수도 있습니다.

## See Also (참고 자료)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [Visual Studio Code](https://code.visualstudio.com/)
- [About Execution Policies](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.security/set-executionpolicy?view=powershell-7.1)
