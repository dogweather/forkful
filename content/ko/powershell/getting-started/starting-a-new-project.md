---
date: 2024-01-20 18:04:21.456880-07:00
description: "\uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD55C\uB2E4\uB294\
  \ \uAC74 \uB9D0 \uADF8\uB300\uB85C \uBE48 \uCE94\uBC84\uC2A4\uC5D0 \uADF8\uB9BC\uC744\
  \ \uADF8\uB9AC\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC544\uC774\uB514\uC5B4\uB97C \uD604\uC2E4\uB85C \uB9CC\uB4E4\
  \uAC70\uB098 \uBB38\uC81C\uB97C \uD574\uACB0\uD558\uAE30 \uC704\uD574 \uC0C8 \uD504\
  \uB85C\uADF8\uB7A8\uC744 \uAC1C\uBC1C\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.547610-06:00'
model: gpt-4-1106-preview
summary: "\uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD55C\uB2E4\uB294 \uAC74\
  \ \uB9D0 \uADF8\uB300\uB85C \uBE48 \uCE94\uBC84\uC2A4\uC5D0 \uADF8\uB9BC\uC744 \uADF8\
  \uB9AC\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
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
